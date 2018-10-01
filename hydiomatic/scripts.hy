#! /usr/bin/env hy
;; hydiomatic -- The Hy Transformer
;; Copyright (C) 2014, 2015, 2016  Gergely Nagy <algernon@madhouse-project.org>
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program. If not, see <http://www.gnu.org/licenses/>.

(import sys
        argparse
        [hy.importer [hy-parse]]
        [hy.cmdline [HyREPL]]
        [hy.completer [completion]]
        [hydiomatic.core [simplify]]
        [hydiomatic.rules [rules/default rules/experimental
                           rules/warnings rules/grand-cleanup
                           rules/jokes]]
        [hydiomatic.utils [hypprint hypformat]]
        [difflib [unified-diff]])

(require [hy.contrib.walk [let]])

(defn parse-file [filename]
  (with [f (open filename "rb")]
    (setv source-str (.decode (.read f) "utf-8"))
    (hy-parse source-str)))

(defn launch-repl []
  (setv sys.ps1 ";=> ")
  (setv sys.ps2 "    ")

  (with [(completion)]
        (setv hr (HyREPL))
        (.runsource hr "(import [hydiomatic.core [*]] [hydiomatic.rules [*]])\n(require [hydiomatic.utils [*]])")
        (.interact hr "hydiomatic")))

(defn process-file [transform printer fn rules]
  (if rules
      (printer #* [(transform (parse-file fn) rules)]
               #** {"outermost" True})
      (printer #* [(transform (parse-file fn))]
               #** {"outermost" True})))

(defn do-diff [fn rules]
  (let [original (process-file identity hypformat fn None)
        simplified (process-file simplify hypformat fn rules)]
    (for [line (unified-diff #* [original simplified]
                             #** {"fromfile" (+ fn ".orig")
                                  "tofile" fn})]
      (sys.stdout.write line))))

(defn pick-rules [experimental? grand-cleanup? jokes?]
  (if jokes?
    rules/jokes
    (if grand-cleanup?
      rules/grand-cleanup
      (if experimental?
        rules/experimental
        rules/default))))

(defn main [&rest args]
  (setv parser (argparse.ArgumentParser #* []
                                        #** {"prog"
                                             "hydiomatic"
                                             "usage"
                                             "%(prog)s [options] FILE"
                                             "formatter_class"
                                             argparse.RawDescriptionHelpFormatter}))

  (parser.add_argument #* ["--repl" "-r"]
         #** {"action" "store_true"
                   "help" "Launch a REPL instead of simplifying a file"})
  (parser.add_argument #* ["--dry-run" "-n"]
         #** {"action" "store_true"
                   "help" "Output the parsed file without simplification"})
  (parser.add_argument #* ["--diff" "-d"]
         #** {"action" "store_true"
                   "help" "Print a unified diff of the original and the simplified file."})
  (parser.add_argument #* ["--experimental" "-e"]
         #** {"action" "store_true"
                   "help" "Use experimental rules too, with potential false positives."})
  (parser.add_argument #* ["--warnings" "-w"]
         #** {"action" "store_true"
                   "help" "Instead of transforming, print warnings that have no transformation."})
  (parser.add_argument #* ["--grand-cleanup" "-g"]
         #** {"action" "store_true"
                   "help" "Use the Grand Cleanup rules too."})
  (parser.add_argument #* ["--jokes" "-j"]
         #** {"action" "store_true"
                   "help" "Use joke rules only."})
  (parser.add_argument #* ["args"]
         #** {"nargs" argparse.REMAINDER
                  "help" argparse.SUPPRESS})

  (setv options (.parse_args parser (rest sys.argv)))

  (cond
   [options.repl (launch-repl)]

   [(and (!= (len options.args) 1)
         (not options.diff))
    (do
     (.print_help parser)
     (sys.exit 1))]

   [options.dry-run
    (process-file (fn [form rules] form) hypprint (first options.args)
                  (pick-rules options.experimental
                              options.grand_cleanup
                              options.jokes))]

   [options.warnings
    (process-file simplify (fn [_ &optional [outermost None]]) (first options.args)
                  rules/warnings)]

   [options.diff
    (for [f options.args]
      (do-diff f (pick-rules options.experimental
                             options.grand_cleanup
                             options.jokes)))]

   [True
    (process-file simplify hypprint (first options.args)
                  (pick-rules options.experimental
                              options.grand_cleanup
                              options.jokes))]))

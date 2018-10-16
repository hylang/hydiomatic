;; hydiomatic -- The Hy Transformer
;; Copyright (C) 2014, 2015  Gergely Nagy <algernon@madhouse-project.org>
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

(import hy.contrib.walk
        [adderall.dsl [*]]
        [adderall.internal [ConsPair car cdr]])
(import [hydiomatic.rules [*]])

(require [hy.contrib.walk [let]])
(require [hy.extra.anaphoric [*]])
(require [adderall.dsl [*]])


;; Patch the original `walk` so that it handles cons pairs.
(setv orig-walk hy.contrib.walk.walk)
(defn cons-walk [inner outer form]
  "A cons-aware version of `walk`"
  (if (instance? ConsPair form)
      (outer (ConsPair (inner (car form))
                       (inner (cdr form))))
      (orig-walk inner outer form)))
(setv hy.contrib.walk.walk cons-walk)

(import [hy.contrib.walk [prewalk]])

(defn simplify-step-by-rule [rule expr]
  (let [alts (run* [q] (rule expr q))]
    (if (empty? alts)
      expr
      (first alts))))

(defn cleanup-step [expr]
  (if (iterable? expr)
    (simplify-step-by-rule rules/grand-cleanup-finishᵒ
                           expr)
    expr))

(defn cleanup [expr]
  (simplify* expr [rules/grand-cleanup-finishᵒ]))

(defn simplify-step* [expr &optional [rules rules/default]]
  (if (iterable? expr)
    (do
     (setv new-expr expr)
     (for [rule rules]
       (setv new-expr (simplify-step-by-rule rule new-expr)))
     new-expr)
    expr))

(defn simplify-step [expr &optional [rules rules/default]]
  (cleanup-step (simplify-step* expr rules)))

(defn simplify* [expr &optional [rules rules/default]]
  (setv new-expr (prewalk #%(simplify-step* %1 rules) expr))
  (unless (= new-expr expr)
    (while True
      (setv res (prewalk #%(simplify-step* %1 rules) new-expr))
      (when (= res new-expr)
        (break))
      (setv new-expr res)))
  new-expr)

(defn simplify [expr &optional [rules rules/default]]
  (cleanup (simplify* expr rules)))

(defn simplifications [expr &optional [rules rules/default]]
  (setv stages [expr])
  (setv new-expr (prewalk #%(simplify-step* %1 rules) expr))
  (unless (= new-expr expr)
    (.append stages (cleanup-step new-expr))
    (while True
      (setv res (prewalk #%(simplify-step* %1 rules) new-expr))
      (when (= res new-expr)
        (break))
      (setv new-expr res)
      (.append stages (cleanup-step new-expr))))
  stages)

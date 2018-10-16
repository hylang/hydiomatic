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

(import [hy [HyString]]
        [adderall.dsl [*]]
        [adderall.extra.misc [*]])

(require [adderall.dsl [*]])
(require [hydiomatic.macros [*]])


(defrules [rules/control-structᵒ rules/control-structo]
  ;; (if test y None) => (when test y)
  [`(if ~?test ~?yes-branch None)
   `(when ~?test ~?yes-branch)]

  ;; (if test None n) => (unless test n)
  [`(if ~?test None ~?no-branch)
   `(unless ~?test ~?no-branch)]

  ;; (if (not test) a b) => (if-not test a b)
  [(cons `if `(not ~?test) ?branches)
   (cons `if-not ?test ?branches)]

  ;; (if test (do y)) => (when test y)
  [`(if ~?test ~(cons `do ?y))
   (cons `when ?test ?y)]

  ;; (when (not test) stuff) => (unless test stuff)
  [(cons `when `(not ~?test) ?body)
   (cons `unless ?test ?body)]

  ;; (do x) => x
  [`(do ~?body) ?body]

  ;; (when test (do x)) => (when test x)
  [`(when ~?test ~(cons `do ?body))
   (cons `when ?test ?body)]

  ;; (unless test (do x)) => (unless test x)
  [`(unless ~?test ~(cons `do ?body))
   (cons `unless ?test ?body)]

  ;; (fn [...] (do x)) => (fn [...] x)
  [`(fn ~?args ~(cons `do ?body))
   (cons `fn ?args ?body)]

  ;; (fn [...] "docstring" (do x)) => (fn [...] "docstring" x)
  [`(fn ~?args ~?docstring ~(cons `do ?body))
   (cons `fn ?args ?docstring ?body)]

  ;; (try (do ...)) => (try ...)
  [`(try ~(cons `do ?body)) (cons `try ?body)]

  ;; (defn [...] (do x)) => (defn [...] x)
  ;; (defun [...] (do x)) => (defun [...] x)
  ;; (defn [...] "..." (do x)) => (defn "..." [...] x)
  ;; (defun [...] "..." (do x)) => (defun "..." [...] x)
  (prep
   (condᵉ
    [(≡ expr `(~?op ~?name ~?args ~(cons `do ?body)))
     (≡ out (cons ?op ?name ?args ?body))]
    [(≡ expr `(~?op ~?name ~?args ~?docstring ~(cons `do ?body)))
     (typeᵒ ?docstring HyString)
     (≡ out (cons ?op ?name ?args ?docstring ?body))])
   (memberᵒ ?op `[defn defun defn-alias defun-alias]))

  ;; (if test a) => (when test a)
  ;; (if-not test a) => (unless test a)
  ;; unless a is an unquote-splice
  (prep
   (≡ expr `(~?op ~?test ~?branch))
   (condᵉ
    [(≡ ?op `if) (≡ ?new-op `when)]
    [(≡ ?op `if-not) (≡ ?new-op `unless)])
   (condᵉ
    [(firstᵒ ?branch `unquote-splice) (≡ out expr)]
    (else (≡ out `(~?new-op ~?test ~?branch)))))

  ;; (let [...] (do ...)) => (let [...] ...)
  [`(let ~?bindings ~(cons `do ?exprs))
   (cons `let ?bindings ?exprs)]

  ;; (loop [...] (do ...)) => (loop [...] ...)
  [`(loop ~?bindings ~(cons `do ?exprs))
   (cons `loop ?bindings ?exprs)]

  ;; (loop [] (when ... (recur))) => (while ... ...)
  (prep
   (≡ expr `(loop [] ~(cons `when ?test ?body)))
   (appendᵒ ?exprs [`(recur)] ?body)
   (project [?exprs ?test]
            (≡ out (HyExpression (cons `while ?test ?exprs)))))

  ;; (while True (yield func)) => (repeatedly func)
  [`(while True (yield ~?func)) `(repeatedly ~?func)])

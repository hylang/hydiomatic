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

(import hy)
(import [adderall.dsl [*]])

(require [adderall.dsl [*]])
(require [hydiomatic.macros [*]])


(defrules [rules/arithmeticᵒ rules/arithmetico]
  ;; (+ 0 x), (+ x 0) => x
  [`(+ 0 ~?x) ?x]
  [`(+ ~?x 0) ?x]

  ;; (* 1 x), (* x 1) => x
  [`(* 1 ~?x) ?x]
  [`(* ~?x 1) ?x]

  ;; (+ x (+ ...)) => (+ x ...)
  [`(+ ~?x ~(cons '+ ?xs)) (cons '+ ?x ?xs)]

  ;; (* x (* ...)) => (* x ...)
  [`(* ~?x ~(cons '* ?xs)) (cons '* ?x ?xs)]

  ;; (+ x 1), (+ 1 x) => (inc x)
  [`(+ ~?x 1) `(inc ~?x)]
  [`(+ 1 ~?x) `(inc ~?x)]

  ;; (- x 1) => (dec x)
  [`(- ~?x 1) `(dec ~?x)])

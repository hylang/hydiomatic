;; hydiomatic -- The Hy Transformer
;; Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
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

(import [adderall.dsl [*]])
(require adderall.dsl)
(require hydiomatic.macros)

(defrules [rules/arithmeticᵒ rules/arithmetico]
  ;; (+ 0 x), (+ x 0) => x
  (rule [x] `(+ 0 ~x) x)
  (rule [x] `(+ ~x 0) x)
  ;; (* 1 x), (* x 1) => x
  (rule [x] `(* 1 ~x) x)
  (rule [x] `(* ~x 1) x)
  ;; (+ x (+ ...)) => (+ x ...)
  (rule [x xs] `(+ ~x (+ . ~xs)) `(+ ~x . ~xs))
  ;; (* x (* ...)) => (* x ...)
  (rule [x xs] `(* ~x (* . ~xs)) `(* ~x . ~xs))
  ;; (+ x 1), (+ 1 x) => (inc x)
  (rule [x] `(+ ~x 1) `(inc ~x))
  (rule [x] `(+ 1 ~x) `(inc ~x))
  ;; (- x 1) => (dec x)
  (rule [x] `(- ~x 1) `(dec ~x)))

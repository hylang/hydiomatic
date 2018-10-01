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

(import [adderall.dsl [*]])

(require [adderall.dsl [*]])
(require [hydiomatic.macros [*]])


(defrules [rules/equalityᵒ rules/equalityo]
  ;; (= (% n 2) 0) => (even? n)
  [`(= (% ~?n 2) 0) `(even? ~?n)]

  ;; (= (% n 2) 1) => (odd? n)
  [`(= (% ~?n 2) 1) `(odd? ~?n)]

  ;; zero?
  [`(= 0 ~?x) `(zero? ~?x)]
  [`(= ~?x 0) `(zero? ~?x)]

  ;; pos?
  [`(< 0 ~?x) `(pos? ~?x)]
  [`(> ~?x 0) `(pos? ~?x)]

  ;; neg?
  [`(< ~?x 0) `(neg? ~?x)]

  ;; none?
  [`(is ~?x None) `(none? ~?x)]
  [`(is None ~?x) `(none? ~?x)]

  ;; nil? => none?
  [`(nil? ~?x) `(none? ~?x)]

  ;; (not (is ...)) => (is-not ...)
  [`(not ~(cons `is ?xs)) (cons `is-not ?xs)]

  ;; (not (= ...)) => (!= ...)
  [`(not ~(cons `= ?xs)) (cons `!= ?xs)]

  ;; (not (in ...)) => (not-in ...)
  [`(not ~(cons `in ?xs)) (cons `not-in ?xs)]

  ;; (if-not (is ...) ...) => (if (is-not ...) ...)
  [(cons `if-not (cons `is ?xs) ?ys)
   (cons `if (cons `is-not ?xs) ?ys)])

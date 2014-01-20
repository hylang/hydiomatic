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

(import [hydiomatic.core [*]])

(defn test-simplification-arith-inc []
  (assert (= (simplify-expression '(+ 2 1))
             '(inc 2)))
  (assert (= (simplify-expression '(+ 1 2))
             '(inc 2))))

(defn test-simplification-arith-dec []
  (assert (= (simplify-expression '(- 3 1))
             '(dec 3))))

(defn test-simplification-arith-times []
  (assert (= (simplify-expression '(* 2 (* 3 4)))
             '(* 2 3 4))))

(defn test-simplification-arith-many-plus []
  (assert (= (simplify-expression '(+ 1 (+ 2 3)))
             '(+ 1 2 3))))

(defn test-simplification-arith-plus-zero []
  (assert (= (simplify-expression '(+ 0 1))
             '1))
  (assert (= (simplify-expression '(+ 1 0))
             '1)))

(defn test-simplification-arith-times-one []
  (assert (= (simplify-expression '(* 1 2))
             '2))
  (assert (= (simplify-expression '(* 2 1))
             '2)))

(defn test-simplifyication-quote-qq-unquote []
  (assert (= (simplify-expression '`~x)
             'x)))

(defn test-simplification-identity []
  (assert (= (simplify-expression '())
             '()))
  (assert (= (simplify-expression '(inc 2))
             '(inc 2))))

(defn test-simplify []
  (assert (= (simplify '(do (+ 1 (+ 1))))
             '(do (inc 1))))
  (assert (= (simplify '(* 2 (* 3 (+ 5 (+ 1)))))
             '(* 2 3 (inc 5))))
  (assert (= (simplify '(* a (* b (+ c (+ 1)))))
             '(* a b (inc c))))
  (assert (= (simplify '[a b (+ 2 1) `~x])
             '[a b (inc 2) x])))

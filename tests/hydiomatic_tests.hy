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

(defn test-rules-arithmetico []
  (assert (= (simplify-expression '(+ 2 1))
             '(inc 2)))
  (assert (= (simplify-expression '(+ 1 2))
             '(inc 2)))
  (assert (= (simplify-expression '(- 3 1))
             '(dec 3)))
  (assert (= (simplify-expression '(* 2 (* 3 4)))
             '(* 2 3 4)))
  (assert (= (simplify-expression '(+ 1 (+ 2 3)))
             '(+ 1 2 3)))
  (assert (= (simplify-expression '(+ 0 1))
             '1))
  (assert (= (simplify-expression '(+ 1 0))
             '1))
  (assert (= (simplify-expression '(* 1 2))
             '2))
  (assert (= (simplify-expression '(* 2 1))
             '2)))

(defn test-rules-quoteo []
  (assert (= (simplify-expression '`~x)
             'x)))

(defn test-rules-control-structo []
  (assert (= (simplify-expression '(if true :yes nil))
             '(when true :yes)))
  (assert (= (simplify-expression '(if true nil :no))
             '(unless true :no)))
  (assert (= (simplify-expression '(if true (do (and this that))))
             '(when true (and this that))))
  (assert (= (simplify-expression '(when (not true) :hello))
             '(unless true :hello)))
  (assert (= (simplify-expression '(do something))
             'something))
  (assert (= (simplify-expression '(when true (do stuff)))
             '(when true stuff)))
  (assert (= (simplify-expression '(unless true (do stuff)))
             '(unless true stuff))))

(defn test-simplification-equalityo []
  (assert (= (simplify-expression '(= 0 x))
             '(zero? x)))
  (assert (= (simplify-expression '(= x 0))
             '(zero? x)))
  (assert (= (simplify-expression '(< 0 x))
             '(pos? x)))
  (assert (= (simplify-expression '(> x 0))
             '(pos? x)))
  (assert (= (simplify-expression '(< x 0))
             '(neg? x)))
  (assert (= (simplify-expression '(= x nil))
             '(nil? x)))
  (assert (= (simplify-expression '(= nil x))
             '(nil? x))))

(defn test-rules-none []
  (assert (= (simplify-expression '())
             '()))
  (assert (= (simplify-expression '(inc 2))
             '(inc 2))))

(defn test-simplify []
  (assert (= (simplify '(something (+ 1 (+ 1))))
             '(something (inc 1))))
  (assert (= (simplify '(* 2 (* 3 (+ 5 (+ 1)))))
             '(* 2 3 (inc 5))))
  (assert (= (simplify '(* a (* b (+ c (+ 1)))))
             '(* a b (inc c))))
  (assert (= (simplify '[a b (+ 2 1) `~x])
             '[a b (inc 2) x]))
  (assert (= (simplify '(if true (do this) (do that)))
             '(if true this that))))

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

(import [hydiomatic.core [*]]
        [hy [HyDict]])

(defn test-rules-arithmetico []
  (assert (= (simplify-step '(+ 2 1))
             '(inc 2)))
  (assert (= (simplify-step '(+ 1 2))
             '(inc 2)))
  (assert (= (simplify-step '(- 3 1))
             '(dec 3)))
  (assert (= (simplify-step '(* 2 (* 3 4)))
             '(* 2 3 4)))
  (assert (= (simplify-step '(+ 1 (+ 2 3)))
             '(+ 1 2 3)))
  (assert (= (simplify-step '(+ 0 1))
             '1))
  (assert (= (simplify-step '(+ 1 0))
             '1))
  (assert (= (simplify-step '(* 1 2))
             '2))
  (assert (= (simplify-step '(* 2 1))
             '2)))

(defn test-rules-quoteo []
  (assert (= (simplify-step '`~x)
             'x)))

(defn test-rules-control-structo []
  (assert (= (simplify-step '(if true :yes nil))
             '(when true :yes)))
  (assert (= (simplify-step '(if true nil :no))
             '(unless true :no)))
  (assert (= (simplify-step '(if true (do (and this that))))
             '(when true (and this that))))
  (assert (= (simplify-step '(when (not true) :hello))
             '(unless true :hello)))
  (assert (= (simplify-step '(do something))
             'something))
  (assert (= (simplify-step '(when true (do stuff)))
             '(when true stuff)))
  (assert (= (simplify-step '(unless true (do stuff)))
             '(unless true stuff)))
  (assert (= (simplify-step '(if (not true) a b))
             '(if-not true a b)))
  (assert (= (simplify-step '(if (not true) a))
             '(if-not true a)))
  (assert (= (simplify-step '(if-not true a))
             '(unless true a)))
  (assert (= (simplify-step '(if true a))
             '(when true a))))

(defn test-rules-equalityo []
  (assert (= (simplify-step '(= 0 x))
             '(zero? x)))
  (assert (= (simplify-step '(= x 0))
             '(zero? x)))
  (assert (= (simplify-step '(< 0 x))
             '(pos? x)))
  (assert (= (simplify-step '(> x 0))
             '(pos? x)))
  (assert (= (simplify-step '(< x 0))
             '(neg? x)))
  (assert (= (simplify-step '(= n nil))
             '(nil? n)))
  (assert (= (simplify-step '(= nil n))
             '(nil? n)))
  (assert (= (simplify-step '(= (% n 2) 0))
             '(even? n)))
  (assert (= (simplify-step '(= (% n 2) 1))
             '(odd? n)))
  (assert (= (simplify-step '(none? n))
             '(nil? n))))

(defn test-rules-collectiono []
  (assert (= (simplify-step '(get coll 0))
             '(first coll)))
  (assert (= (simplify-step '(slice coll 1))
             '(rest coll)))
  (assert (= (simplify-step '(slice coll 2))
             '(slice coll 2)))
  (assert (= (simplify-step '(zero? (len coll)))
             '(empty? coll))))

(defn test-rules-syntaxo []
  (assert (= (simplify-step '(defn foo (a b c) (+ a b c)))
             '(defn foo [a b c]
                (+ a b c))))
  (let [[alt (simplify-step '(defn foo (a b c) (+ a b c)))]]
    (assert (= (type (get alt 2))
               HyList)))
  (let [[alt (simplify-step '(defun foo (a b c) (+ a b c)))]]
    (assert (= (type (get alt 2))
               HyList)))
  (let [[alt (simplify-step '(defn-alias [foo bar] (a b c) (+ a b c)))]]
    (assert (= (type (get alt 2))
               HyList)))
  (let [[alt (simplify-step '(defun-alias [foo bar] (a b c) (+ a b c)))]]
    (assert (= (type (get alt 2))
               HyList))))

(defn test-rules-optimo []
  (assert (= (simplify-step '(defn foo [x]
                               (let [[y (inc x)]]
                                 (print x y))))
             '(defn foo [x]
                (setv y (inc x))
                (print x y))))
  (assert (= (simplify-step '(defun foo [x]
                               (let [[y (inc x)]]
                                 (print x y))))
             '(defun foo [x]
                (setv y (inc x))
                (print x y))))
  (assert (= (simplify-step '(defn-alias [foo bar] [x]
                               (let [[y (inc x)]]
                                 (print x y))))
             '(defn-alias [foo bar] [x]
                (setv y (inc x))
                (print x y))))
  (assert (= (simplify-step '(defun-alias [foo bar] [x]
                               (let [[y (inc x)]]
                                 (print x y))))
             '(defun-alias [foo bar] [x]
                (setv y (inc x))
                (print x y))))
  (assert (= (simplify-step '(defn foo [x a &optional [foo 'bar]]
                               (let [[y (inc x)]]
                                 (print x y))))
             '(defn foo [x a &optional [foo 'bar]]
                (setv y (inc x))
                (print x y))))
  (assert (= (simplify-step '(defn foo [x]
                               (let [[y (inc x)] [z (inc y)]]
                                 (print x y)
                                 (+ x y z))))
             '(defn foo [x]
                (setv y (inc x))
                (setv z (inc y))
                (print x y)
                (+ x y z)))))

(defn test-rules-none []
  (assert (= (simplify-step '())
             '()))
  (assert (= (simplify-step '(inc 2))
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
             '(if true this that)))
  (assert (= (simplify '{"foo" "bar"})
             '{"foo" "bar"}))
  (assert (= (type (simplify '{"foo" "bar"}))
             HyDict))
  (assert (= (simplify '(def a {"foo" (+ 1 1)}))
             '(def a {"foo" (inc 1)})))
  (assert (= (simplify '(= (len coll) 0))
             '(empty? coll))))

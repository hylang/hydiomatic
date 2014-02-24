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
        [hydiomatic.rules [*]]
        [hy [HyDict HyList]])

(defmacro assert-step [expr expected]
  `(assert (= (simplify-step '~expr rules/experimental)
              '~expected)))

(defmacro assert-simplify [expr expected]
  `(assert (= (simplify '~expr rules/experimental)
              '~expected)))

(defmacro/g! wrap-stdout [&rest body]
  `(do
    (import [sys] [io [StringIO]])
    (setv ~g!old-stdout sys.stdout)
    (setv sys.stdout (StringIO))
    (setv ~g!result (do ~@body))
    (setv ~g!stdout (.getvalue sys.stdout))
    (setv sys.stdout ~g!old-stdout)
    [~g!stdout ~g!result]))

(defn test-rules-arithmetico []
  (assert-step (+ 2 1) (inc 2))
  (assert-step (+ 1 2) (inc 2))
  (assert-step (- 3 1) (dec 3))
  (assert-step (* 2 (* 3 4)) (* 2 3 4))
  (assert-step (+ 1 (+ 2 3)) (+ 1 2 3))
  (assert-step (+ 0 1) 1)
  (assert-step (+ 1 0) 1)
  (assert-step (* 1 2) 2)
  (assert-step (* 2 1) 2))

(defn test-rules-quoteo []
  (assert-step `~x x))

(defn test-rules-control-structo []
  (assert-step (if true :yes nil)
               (when true :yes))
  (assert-step (if true nil :no)
               (unless true :no))
  (assert-step (if true (do (and this that)))
               (when true (and this that)))
  (assert-step (if true ~@a)
               (if true ~@a))
  (assert-step (when (not true) :hello)
               (unless true :hello))
  (assert-step (do something)
               something)
  (assert-step (when true (do stuff))
               (when true stuff))
  (assert-step (unless true (do stuff))
               (unless true stuff))
  (assert-step (if (not true) a b)
               (if-not true a b))
  (assert-step (if (not true) a)
               (if-not true a))
  (assert-step (if-not true a)
               (unless true a))
  (assert-step (if-not true ~@a)
               (if-not true ~@a))
  (assert-step (fn [a b c] (do (+ a b c) (inc a)))
               (fn [a b c] (+ a b c) (inc a)))
  (assert-step (fn [a b c] "This is my docstring!"
                 (do (+ a b c) (inc a)))
               (fn [a b c] "This is my docstring!"
                 (+ a b c) (inc a)))
  (assert-step (try (do (something) (catch [e Exception] (pass))))
               (try (something) (catch [e Exception] (pass))))
  (assert-step (defn foo [a b c] (do (+ a b c) (inc a)))
               (defn foo [a b c] (+ a b c) (inc a)))
  (assert-step (defn foo [a b c] "This is my docstring!"
                 (do (+ a b c) (inc a)))
               (defn foo [a b c] "This is my docstring!"
                 (+ a b c) (inc a)))
  (assert-step (defn foo [a b c] something (do (+ a b c) (inc a)))
               (defn foo [a b c] something (do (+ a b c) (inc a))))
  (assert-step (if true a)
               (when true a))
  (assert-step (let [[a 1] [b 2]]
                 (do (print a b)
                     (+ a b)))
               (let [[a 1] [b 2]]
                 (print a b)
                 (+ a b)))
  (assert-step (loop [[i 1]]
                     (do
                      (print i)
                      (when (< i 10)
                        (recur (inc i)))))
               (loop [[i 1]]
                     (print i)
                     (when (< i 10)
                       (recur (inc i)))))
  (assert-step (loop [] (when true (print "zing") (recur)))
               (while true (print "zing"))))

(defn test-rules-equalityo []
  (assert-step (= 0 x) (zero? x))
  (assert-step (= x 0) (zero? x))
  (assert-step (< 0 x) (pos? x))
  (assert-step (> x 0) (pos? x))
  (assert-step (< x 0) (neg? x))
  (assert-step (is n nil) (nil? n))
  (assert-step (is nil n) (nil? n))
  (assert-step (none? n) (nil? n))
  (assert-step (= (% n 2) 0) (even? n))
  (assert-step (= (% n 2) 1) (odd? n))
  (assert-step (not (is a b c)) (is-not a b c))
  (assert-step (not (= a b c)) (!= a b c))
  (assert-step (not (in a b c)) (not-in a b c))
  (assert-step (if-not (is condition False) 'yes 'no)
               (if (is-not condition False) 'yes 'no)))

(defn test-rules-collectiono []
  (assert-step (get coll 0) (first coll))
  (assert-step (get coll 1) (second coll))
  (assert-step (slice coll 1) (rest coll))
  (assert-step (slice coll 2) (slice coll 2))
  (assert-step (zero? (len coll)) (empty? coll)))

(defn test-rules-syntaxo []
  (assert-step (defn foo (a b c) (+ a b c))
               (defn foo [a b c] (+ a b c)))
  (assert-step (defn foo (a b c) "docstring!" (+ a b c))
               (defn foo [a b c] "docstring!" (+ a b c)))
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
               HyList)))
  (let [[alt (simplify-step '(defn foo (a b c) "docstring!" (+ a b c)))]]
    (assert (= (type (get alt 2))
               HyList)))

  (assert-step (isinstance x foo) (instance? foo x))
  (assert-step (instance? float x) (float? x))
  (assert-step (instance? int x) (integer? x))
  (assert-step (instance? str x) (string? x))
  (assert-step (instance? unicode x) (string? x))

  (assert-step (for* [x coll] (yield x))
               (yield-from coll))

  (assert-step (-> (-> a b) c d)
               (-> a b c d))
  (assert-step (-> a) a)

  (assert-step (kwapply (.method self param1 param2)
                        {"key" "value"})
               (apply self.method [param1 param2]
                      {"key" "value"}))

  (assert-step (kwapply (method param1 param2)
                        {"key" "value"})
               (apply method [param1 param2]
                      {"key" "value"})))

(defn test-rules-optimo []
  (assert-step (defn foo [x]
                 (let [[y (inc x)]]
                   (print x y)))
               (defn foo [x]
                 (setv y (inc x))
                 (print x y)))
  (assert-step (defun foo [x]
                 (let [[y (inc x)]]
                   (print x y)))
               (defun foo [x]
                 (setv y (inc x))
                 (print x y)))
  (assert-step (defn-alias [foo bar] [x]
                 (let [[y (inc x)]]
                   (print x y)))
               (defn-alias [foo bar] [x]
                 (setv y (inc x))
                 (print x y)))
  (assert-step (defun-alias [foo bar] [x]
                 (let [[y (inc x)]]
                   (print x y)))
               (defun-alias [foo bar] [x]
                 (setv y (inc x))
                 (print x y)))
  (assert-step (defn foo [x a &optional [foo 'bar]]
                 (let [[y (inc x)]]
                   (print x y)))
               (defn foo [x a &optional [foo 'bar]]
                 (setv y (inc x))
                 (print x y)))
  (assert-step (defn foo [x]
                 (let [[y (inc x)] [z (inc y)]]
                   (print x y)
                   (+ x y z)))
               (defn foo [x]
                 (setv y (inc x))
                 (setv z (inc y))
                 (print x y)
                 (+ x y z)))
  (assert-step (defn foo [x]
                 "This is my docstring"
                 (let [[y (inc x)]]
                   (print x y)))
               (defn foo [x]
                 "This is my docstring"
                 (setv y (inc x))
                 (print x y)))
  (assert-step (defn foo [x]
                 (make-something)
                 (let [[y (inc x)]]
                   (print x y)))
               (defn foo [x]
                 (make-something)
                 (let [[y (inc x)]]
                   (print x y))))

  (assert-step (fn [x] (nil? x))
               nil?)
  (assert-step (fn [x] (+ x 2))
               (fn [x] (+ x 2)))
  (assert-step (fn [a b] (mix a b))
               mix)
  (assert-step (fn [a b] "a docstring" (mix a b))
               mix)
  (assert-step (fn [a b] (+ a b))
               (fn [a b] (+ a b)))
  (assert-step (lambda [x] (frob x))
               frob))

(defn test-rules-none []
  (assert-step () ())
  (assert-step (inc 2) (inc 2))
  (assert-step [a] [a])
  (assert (= (type (simplify '[]))
             HyList)))

(defn test-simplify []
  (assert-simplify (something (+ 1 (+ 1)))
                   (something (inc 1)))
  (assert-simplify (* 2 (* 3 (+ 5 (+ 1))))
                   (* 2 3 (inc 5)))
  (assert-simplify (* a (* b (+ c (+ 1))))
                   (* a b (inc c)))
  (assert-simplify [a b (+ 2 1) `~x]
                   [a b (inc 2) x])
  (assert-simplify (if true (do this) (do that))
                   (if true this that))
  (assert-simplify (def a {"foo" (+ 1 1)})
                   (def a {"foo" (inc 1)}))
  (assert-simplify (= (len coll) 0)
                   (empty? coll))
  (assert-simplify (defmacro if-truth [test &rest branches]
                     (if (not (is ~test)) ~@branches))
                   (defmacro if-truth [test &rest branches]
                     (if (is-not ~test) ~@branches)))

  (assert-simplify {"foo" "bar"} {"foo" "bar"})
  (assert (= (type (simplify '{"foo" "bar"}))
             HyDict)))

(defn test-warnings []
  (assert (= (wrap-stdout
              (simplify-step '(defn nodocs [a] (inc a))
                             rules/warnings))
             ["; Function `nodocs` has no docstring.\n"
              `(defn nodocs [a] (inc a))]))

  (assert (= (wrap-stdout
              (simplify-step '(fn [a, b] (+ a b))
                             rules/warnings))
             ["; In `(fn [a, b] (+ a b))`, you may want to use `a` instead of `a,` in the arglist.\n"
              `(fn [a, b] (+ a b))]))

  (assert (= (wrap-stdout
              (simplify-step '(fresh [f r]
                                     (firstᵒ l f)
                                     (restᵒ l r))
                             rules/warnings))
             ["; Instead of `(firstᵒ l f)` and `(restᵒ l r)`, consider using `(consᵒ f r l)`.\n"
              `(fresh [f r]
                      (firstᵒ l f)
                      (restᵒ l r))])))

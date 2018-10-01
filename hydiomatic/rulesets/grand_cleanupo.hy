;; hydiomatic -- The Hy Transformer
;; Copyright (C) 2015  Gergely Nagy <algernon@madhouse-project.org>
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

(import [adderall.dsl [*]]
        [adderall.extra.misc [*]]
        [hy [HySymbol HyList HyExpression]])

(require [adderall.dsl [*]])
(require [hydiomatic.macros [*]])


(defn simple-flatten [coll]
  (setv new-coll [])
  (for [member coll]
    (.extend new-coll member))
  new-coll)

(defn transform-bindingᵒ [in out]
  (prep
   (condᵉ [(typeᵒ in HyList) (≡ out in)]
          (else (≡ out `[~in None])))))

(defn transform-conditionᵒ [in out]
  (prep
   (consᵒ ?test ?effects in)
   (firstᵒ ?effects ?effect)
   (condᵉ
    [(≡ ?effect (cons 'do ?body))
     (≡ ?result (cons ?test ?body))
     (project [?result]
              (≡ out (HyList ?result)))]
    (else (≡ in out)))))

(defn transform-listᵒ [transformᵒ in out]
  (prep
   (condᵉ [(emptyᵒ in) (≡ in out)]
          [(consᵒ ?first ?rest in)
           (transformᵒ ?first ?new-first)
           (transform-listᵒ transformᵒ ?rest ?new-rest)
           (consᵒ ?new-first ?new-rest out)])))

(defn partition-vars-and-fns [body]
  (setv vars [])
  (setv fns [])
  (for [expr body]
    (if (and (>= (len expr) 2)
             (coll? (second expr))
             (= (first (second expr)) `fn))
      (.append fns expr)
      (.append vars expr)))
  (, vars fns))

(defn transform-defnᵒ [in out]
  (prep
   (≡ in `[~?name ~(cons 'fn ?body)])
   (≡ out (cons 'defn ?name ?body))))

(defrules [rules/grand-cleanupᵒ rules/grand-cleanupo]
  ;; (let [[x 1] [y 2] z] ...) => (let [x 1 y 2 z None] ...)
  ;; (with [[x 1] [y 2] z] ...) => (with [x 1 y 2 z None] ...)
  (prep
   (≡ expr (cons ?op ?bindings ?body))
   (memberᵒ ?op `[let with])
   (transform-listᵒ transform-bindingᵒ ?bindings ?new-bindings)
   (project [?new-bindings]
            (≡ ?flat-bindings (simple-flatten ?new-bindings)))
   (condᵉ
    [(≡ ?op `let) (≡ ?new-op `$hydiomatic/let$)]
    [(≡ ?op `with) (≡ ?new-op `$hydiomatic/with$)])
   (≡ out (cons ?new-op ?flat-bindings ?body)))

  ;; (for [...] (do ...)) => (for [...] ...)
  [`(for ~?bindings ~(cons 'do ?body))
   (cons 'for ?bindings ?body)]

  ;; (cond [(test) (do ...)]
  ;;       [(test2) (effect)])
  ;; =>
  ;; (cond [(test) ...]
  ;;       [(test2) (effect)])
  (prep
   (≡ expr (cons 'cond ?conditions))
   (transform-listᵒ transform-conditionᵒ ?conditions ?new-conditions)
   (≡ out (cons 'cond ?new-conditions)))

  ;; (defclass A [...]
  ;;   [[x 1]
  ;;    [y 2]
  ;;    [foo (fn [self] ...)]])
  ;; =>
  ;; (defclass A [...]
  ;;   [x 1
  ;;    y 2]
  ;;   (defn foo [self] ...))
  ;; + same with docstring
  (prep
   (condᵉ
    [(≡ expr `(defclass ~?name ~?base-list
                ~?body))]
    [(≡ expr `(defclass ~?name ~?base-list
                ~?docstring
                ~?body))])
   (project [?body]
            (≡ (, ?vars ?fns) (partition-vars-and-fns ?body)))
   (project [?vars ?fns]
            (≡ ?new-vars (simple-flatten ?vars))
            (transform-listᵒ transform-defnᵒ ?fns ?new-fns))
   (condᵉ
    [(emptyᵒ ?docstring)
     (≡ ?new-form (cons '$hydiomatic/defclass$ ?name ?base-list
                    ?new-vars ?new-fns))]
    (else
     (≡ ?new-form (cons '$hydiomatic/defclass$ ?name ?base-list
                    ?docstring
                    ?new-vars ?new-fns))))
   (project [?new-form]
            (≡ out (HyExpression ?new-form))))

  ;; TODO: Add a test!
  [`(require ~?lib) `(require [~?lib [*]])]

  ;; TODO: Add a test!
  [`(import [~?lib]) `(import ~?lib)]

  ;; TODO: Add a test!
  [`(list-comp ~?body [~?x ~?bindings])
   `(lfor ~?x ~?bindings ~?body)]

  ;; (slice) is now (cut)
  [(cons 'slice ?body) (cons 'cut ?body)]

  ;; (throw) is now (rise)
  [(cons 'throw ?body) (cons 'raise ?body)]

  ;; (catch) is now (except)
  [(cons 'catch ?body) (cons 'except ?body)]

  ;; (progn) is now (do)
  [(cons 'progn ?body) (cons 'do ?body)]

  ;; (defun) is now (defn)
  [(cons 'defun ?body) (cons 'defn ?body)]

  ;; (def) is now (setv)
  [(cons 'def ?body) (cons 'setv ?body)]

  ;; (lisp-if) and (lisp-if-not) are now (lif) and (lif-not)
  [(cons 'lisp-if ?body) (cons 'lif ?body)]
  [(cons 'lisp-if-not ?body) (cons 'lif-not ?body)]

  ;; null => None
  [`null `None]

  ;; nill => None
  [`nil `None]

  ;; true => True
  [`true `True]

  ;; false => False
  [`false `False]

  ;; zipwith => map
  [`zipwith `map]

  ;; filterfalse => remove
  [`filterfalse `remove]

  ;; (car . cdr) => (cons car cdr)
  ;; XXX FIXME TODO: This will result in a `cons` object dependency!
  [`(~?car . ~?cdr) (cons ?car ?cdr)])

(defrules [rules/grand-cleanup-finishᵒ rules/grand-cleanup-finisho]
  ;; $hydiomatic/let$ => let
  ;; $hydiomatic/with$ => with
  (prep
   (≡ expr (cons ?op ?args))
   (memberᵒ ?op `[$hydiomatic/let$
                  $hydiomatic/with$
                  $hydiomatic/defclass$])
   (condᵉ
     ;; TODO: `let` isn't builtin anymore, so should we include the `require`
     ;; statement?
    [(≡ ?op `$hydiomatic/let$)
     (≡ ?new-op `let)]
    [(≡ ?op `$hydiomatic/with$)
     (≡ ?new-op `with)]
    [(≡ ?op `$hydiomatic/defclass$)
     (≡ ?new-op `defclass)])
   (≡ out (cons ?new-op ?args))))

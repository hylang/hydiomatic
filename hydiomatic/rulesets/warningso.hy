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

(import [adderall.dsl [*]]
        [adderall.extra.misc [*]]
        [hydiomatic.utils [*]]
        [hy [HySymbol HyString]])

(require [adderall.dsl [*]])
(require [adderall.debug [*]])
(require [hydiomatic.macros [*]])


(defn recmemberᵒ [v l]
  (condᵉ
   [(listᵒ l)
    (fresh [f r]
           (consᵒ f r l)
           (condᵉ
            [(recmemberᵒ v f) s#]
            (else (recmemberᵒ v r))))]
   (else (≡ v l))))

(defmacro suggest [expr orig alt]
  `(log (.format "; In `{0}`, you may want to use `{1}` instead of `{2}` in the arglist."
                 (.rstrip (hypformat ~expr) "\n")
                 (.rstrip (hypformat ~alt) "\n")
                 (.rstrip (hypformat ~orig) "\n"))))

(defrules [rules/warningsᵒ rules/warningso]
  ;; (fn [x, y] (foo x y)) => WARN on using x,!
  (prep
   (≡ expr (cons ?op ?vars ?body))
   (memberᵒ ?op `[fn defn defun defmacro])
   (memberᵒ ?x ?vars)
   (typeᵒ ?x HySymbol)
   (project [?x]
            (≡ ?xstripped (.rstrip ?x ","))
            (if (.endswith ?x ",")
              s#
              u#))
   (recmemberᵒ ?xstripped ?body)
   (condᵉ
    [(recmemberᵒ ?x ?body) u#]
    (else s#))
   (project [expr ?x ?xstripped]
            (suggest expr ?x (HySymbol ?xstripped)))
   u#)

  ;; A function without a docstring is a bad function.
  (prep
   (≡ expr (cons ?op ?name ?vars ?body))
   (memberᵒ ?op `[fn defn defun defmacro])
   (consᵒ ?docstring ?rest ?body)
   (project [?docstring ?rest ?name]
            (if (= (type ?docstring) HyString)
              s#
              (log (.format "; Function `{0}` has no docstring."
                            (.rstrip (hypformat ?name))))))
   u#)
  ;; (firstᵒ l f) (restᵒ l r) => (consᵒ f r l)
  (prep
   (condᵉ
    [(≡ ?foexp `(firsto ~?l ~?f))]
    [(≡ ?foexp `(firstᵒ ~?l ~?f))])
   (condᵉ
    [(≡ ?roexp `(resto ~?l ~?r))]
    [(≡ ?roexp `(restᵒ ~?l ~?r))])
   (memberᵒ ?foexp expr)
   (memberᵒ ?roexp expr)
   (project [?foexp ?roexp ?f ?r ?l]
            (log (.format "; Instead of `{0}` and `{1}`, consider using `{2}`."
                          (.rstrip (hypformat ?foexp))
                          (.rstrip (hypformat ?roexp))
                          (.rstrip (hypformat `(consᵒ ~?f ~?r ~?l))))))
   u#)

  ;; CAPITAL symbols are the same as *ear-muffed* ones, and earmuffs
  ;; are more hydiomatic.
  (prep
   (typeᵒ expr HySymbol)
   (project [expr]
            (do
             (if (.isupper expr)
               s#
               u#)))
   (project [expr]
            (≡ ?suggestion (+ "*" (.lower expr) "*")))
   (project [?suggestion]
            (≡ ?suggested-symbol (HySymbol ?suggestion)))
   (project [expr ?suggested-symbol]
            (log (.format "; Instead of `{0}`, consider using `{1}`."
                          (.rstrip (hypformat expr))
                          (.rstrip (hypformat ?suggested-symbol)))))
   u#))

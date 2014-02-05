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

(import [adderall.dsl [*]]
        [adderall.extra.misc [*]]
        [hydiomatic.utils [*]]
        [hy [HySymbol HyString]])
(require adderall.dsl)
(require adderall.debug)
(require hydiomatic.macros)

(defn recmemberᵒ [v l]
  (condᵉ
   [(listᵒ l)
    (fresh [f r]
           (consᵒ f r l)
           (condᵉ
            [(recmemberᵒ v f) #ss]
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
   (≡ expr `(~?op ~?vars . ~?body))
   (memberᵒ ?op `[fn defn defun defmacro])
   (memberᵒ ?x ?vars)
   (typeᵒ ?x HySymbol)
   (project [?x]
            (≡ ?xstripped (.rstrip ?x ","))
            (if (.endswith ?x ",")
              #ss
              #uu))
   (recmemberᵒ ?xstripped ?body)
   (condᵉ
    [(recmemberᵒ ?x ?body) #uu]
    (else #ss))
   (project [expr ?x ?xstripped]
            (suggest expr ?x (HySymbol ?xstripped)))
   #uu)
  ;; A function without a docstring is a bad function.
  (prep
   (≡ expr `(~?op ~?name ~?vars . ~?body))
   (memberᵒ ?op `[fn defn defun defmacro])
   (consᵒ ?docstring ?rest ?body)
   (project [?docstring ?rest ?name]
            (if (= (type ?docstring) HyString)
              #ss
              (log (.format "; Function `{0}` has no docstring."
                            (.rstrip (hypformat ?name))))))
   #uu)
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
   #uu))

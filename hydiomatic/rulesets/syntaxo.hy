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
        [hy [HyExpression HyList]])
(require adderall.dsl)
(require hydiomatic.macros)

(defn-alias [rules/syntaxᵒ rules/syntaxo] [expr out]
  (condᵉ
   ;; (defn foo (x) ...) => (defn foo [x] ...)
   [(fresh [op fname params body]
           (memberᵒ op `[defn defun defn-alias defun-alias])
           (≡ expr `(~op ~fname ~params . ~body))
           (typeᵒ params HyExpression)
           (project [params]
                    (≡ out `(~op ~fname ~(HyList params) . ~body))))]
   ;; (isinstance x klass) => (instance? klass x)
   (rule [x klass] `(isinstance ~x ~klass) `(instance? ~klass ~x))
   ;; (instance? float x) => (float? x)
   (rule [x] `(instance? float ~x) `(float? ~x))
   ;; (instance? int x) => (integer? x)
   (rule [x] `(instance? int ~x) `(integer? ~x))
   ;; (instance? str x) => (string? x)
   (rule [x] `(instance? str ~x) `(string? ~x))
   ;; (instance? unicode x) => (string? x)
   (rule [x] `(instance? unicode ~x) `(string? ~x))
   ;; (for* [x iteratable] (yield x))
   ;;  => (yield-from iteratable)
   (rule [x iteratable]
         `(for* [~x ~iteratable] (yield ~x))
         `(yield-from ~iteratable))
   ;; (-> a) => a
   (rule [a] `(-> ~a) a)
   ;; (-> (-> x) y) => (-> x y)
   [(fresh [inner x y o]
           (≡ expr `(-> ~inner . ~y))
           (≡ inner `(-> . ~x))
           (≡ o `(-> . ~x))
           (typeᵒ inner HyExpression)
           (typeᵒ x HyExpression)
           (typeᵒ y HyExpression)
           (appendᵒ o y out))]))

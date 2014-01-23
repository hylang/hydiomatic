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

(defn-alias [rules/arithmeticᵒ rules/arithmetico] [expr out]
  (condᵉ
   ;; (+ 0 x), (+ x 0) => x
   ;; (* 1 x), (* x 1) => x
   [(fresh [op zero]
           (condᵉ
            [(≡ op '+) (≡ zero 0)]
            [(≡ op '*) (≡ zero 1)])
           (condᵉ
            [(≡ expr `(~op ~zero ~out))]
            [(≡ expr `(~op ~out ~zero))]))]
   ;; (+ x (+ ...)) => (+ x ...)
   ;; (* x (* ...)) => (* x ...)
   [(fresh [op x xs]
           (memberᵒ op `[+ *])
           (≡ expr `(~op ~x (~op . ~xs)))
           (≡ out `(~op ~x . ~xs)))]
   ;; (+ x 1), (+ 1 x) => (inc x)
   [(fresh [x]
           (condᵉ
            [(≡ expr `(+ ~x 1))]
            [(≡ expr `(+ 1 ~x))])
           (≡ out `(inc ~x)))]
   ;; (- x 1) => (dec x)
   [(fresh [x]
           (≡ expr `(- ~x 1))
           (≡ out `(dec ~x)))]))

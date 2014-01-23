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

(defn-alias [rules/equalityᵒ rules/equalityo] [expr out]
  (condᵉ
   ;; (= (% n 2) 0) => (even? n)
   ;; (= (% n 2) 1) => (odd? n)
   [(fresh [n r op]
           (≡ expr `(= (% ~n 2) ~r))
           (condᵉ
            [(≡ r 0) (≡ op 'even?)]
            [(≡ r 1) (≡ op 'odd?)])
           (≡ out `(~op ~n)))]
   ;; zero?
   [(fresh [x]
           (condᵉ
            [(≡ expr `(= 0 ~x))]
            [(≡ expr `(= ~x 0))])
           (≡ out `(zero? ~x)))]
   ;; pos?
   [(fresh [x]
           (condᵉ
            [(≡ expr `(< 0 ~x))]
            [(≡ expr `(> ~x 0))])
           (≡ out `(pos? ~x)))]
   ;; neg?
   [(fresh [x]
           (≡ expr `(< ~x 0))
           (≡ out `(neg? ~x)))]
   ;; nil?
   [(fresh [x]
           (condᵉ
            [(≡ expr `(= ~x nil))]
            [(≡ expr `(= nil ~x))])
           (≡ out `(nil? ~x)))]
   ;; none? => nil?
   [(fresh [x]
           (≡ expr `(none? ~x))
           (≡ out `(nil? ~x)))]))

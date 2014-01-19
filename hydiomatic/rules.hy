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

(defn-alias [rules/arithmeticᵒ rules/arithmetico] [expression out]
  (condᵉ
   ;; (+ x (+ ...)) => (+ x ...)
   [(fresh [x xs]
           (≡ expression `(+ ~x (+ . ~xs)))
           (≡ out `(+ ~x . ~xs)))]
   ;; (* x (* ...)) => (* x ...)
   [(fresh [x xs]
           (≡ expression `(* ~x (* . ~xs)))
           (≡ out `(* ~x . ~xs)))]
   ;; (+ x 1), (+ 1 x) => (inc x)
   [(fresh [x]
           (condᵉ
            [(≡ expression `(+ ~x 1))]
            [(≡ expression `(+ 1 ~x))])
           (≡ out `(inc ~x)))]
   ;; (- x 1) => (dec x)
   [(fresh [x]
           (≡ expression `(- ~x 1))
           (≡ out `(dec ~x)))]))

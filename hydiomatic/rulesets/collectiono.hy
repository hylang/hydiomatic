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

(defn-alias [rules/collectionᵒ rules/collectiono] [expr out]
  (condᵉ
   ;; (get x 0) => (first x)
   [(fresh [x]
           (≡ expr `(get ~x 0))
           (≡ out `(first ~x)))]
   ;; (get x 1) => (second x)
   [(fresh [x]
           (≡ expr `(get ~x 1))
           (≡ out `(second ~x)))]

   ;; (slice x 1) => (rest x)
   [(fresh [x]
           (≡ expr `(slice ~x 1))
           (≡ out `(rest ~x)))]
   ;; (= (len x) 0), (= 0 (len x)), (zero? (len x))
   ;;  => (empty? x)
   [(fresh [x]
           (condᵉ
            [(≡ expr `(= (len ~x) 0))]
            [(≡ expr `(= 0 (len ~x)))]
            [(≡ expr `(zero? (len ~x)))])
           (≡ out `(empty? ~x)))]))

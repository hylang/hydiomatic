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
(require hydiomatic.macros)

(defrules [rules/collectionᵒ rules/collectiono]
  ;; (get x 0) => (first x)
  [[x] `(get ~x 0) `(first ~x)]
  ;; (get x 1) => (second x)
  [[x] `(get ~x 1) `(second ~x)]
  ;; (slice x 1) => (rest x)
  [[x] `(slice ~x 1) `(rest ~x)]
  ;; (= (len x) 0), (= 0 (len x)), (zero? (len x)]
  ;;  => (empty? x)
  [[x] `(= (len ~x) 0) `(empty? ~x)]
  [[x] `(= 0 (len ~x)) `(empty? ~x)]
  [[x] `(zero? (len ~x)) `(empty? ~x)])

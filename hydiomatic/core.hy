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
        [hydiomatic.rules [*]]
        [hy [HyExpression]]
        [functools [partial]])
(require adderall.dsl)

(defn walk [inner outer form]
  (cond
   [(isinstance form HyExpression)
    (outer (HyExpression (map inner form)))]
   [(isinstance form list)
    (list (outer (HyExpression (map inner form))))]
   [true (outer form)]))

(defn identity [f] f)

(defn prewalk [f form]
  (walk (fn [x] (prewalk f x)) identity (f form)))

(defn simplify-step [expr]
  (if (iterable? expr)
    (let [[alts (run* [q]
                      (condᵉ
                       [(rules/arithmeticᵒ expr q)]
                       [(rules/quoteᵒ expr q)]
                       [(rules/control-structᵒ expr q)]
                       [(rules/equalityᵒ expr q)]))]]
      (if (empty? alts)
        expr
        (first alts)))
    expr))

(defn simplify [expr]
  (setv new-expr (prewalk simplify-step expr))
  (while true
    (setv res (prewalk simplify-step new-expr))
    (when (= res new-expr)
      (break))
    (setv new-expr res))
  new-expr)

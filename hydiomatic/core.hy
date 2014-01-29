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
        [hydiomatic.utils [*]])
(require adderall.dsl)

(defn simplify-step [expr &optional [rules rules/default]]
  (if (iterable? expr)
    (let [[alts (run* [q] (rules expr q))]]
      (if (empty? alts)
        expr
        (first alts)))
    expr))

(defn simplify [expr &optional [rules rules/default]]
  (setv new-expr (prewalk (fn [x] (simplify-step x rules)) expr))
  (while true
    (setv res (prewalk (fn [x] (simplify-step x rules)) new-expr))
    (when (= res new-expr)
      (break))
    (setv new-expr res))
  new-expr)

(defn simplifications [expr &optional [rules rules/default]]
  (setv stages [expr])
  (setv new-expr (prewalk (fn [x] (simplify-step x rules)) expr))
  (.append stages new-expr)
  (while true
    (setv res (prewalk (fn [x] (simplify-step x rules)) new-expr))
    (when (= res new-expr)
      (break))
    (setv new-expr res)
    (.append stages new-expr))
  stages)

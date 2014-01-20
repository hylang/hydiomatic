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

(import [hy [HyExpression HySymbol HyInteger HyString]])

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

(defn -pprint [form]
  (cond
   [(isinstance form HyExpression)
    (+ "(" (.join " " (map -pprint form)) ")")]
   [(isinstance form HySymbol)
    (str form)]
   [(isinstance form HyInteger)
    (str form)]
   [(isinstance form HyString)
    (str (+ "\"" (str form) "\""))]
   [(isinstance form dict)
    nil]
   [(isinstance form list)
    (+ "[" (.join " " (map -pprint form)) "]")]
   [true
    nil]))

(defn hypprint [form &optional [outermost false]]
  (if outermost
    (map hypprint form)
    (print (-pprint form))))

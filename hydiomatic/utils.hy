;; hydiomatic -- The Hy Transformer
;; Copyright (C) 2014, 2015  Gergely Nagy <algernon@madhouse-project.org>
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

(import sys)
(import [hy [HyExpression HySymbol HyInteger HyString HyDict
             HyKeyword]])

(require [hy.contrib.walk [let]])


(defn -hystringify [value]
  (let [sv (string value)]
    (if (.startswith sv "is_")
      (+ (cut sv 3) "?")
      sv)))

(defn -pprint [form]
  (cond
   [(instance? HyExpression form)
    (+ "(" (.join " " (map -pprint form)) ")")]
   [(instance? HySymbol form)
    (-hystringify form)]
   [(or (instance? HyInteger form) (integer? form))
    (string form)]
   [(instance? HyKeyword form)
    (-hystringify (rest (rest form)))]
   [(or (instance? HyString form) (string? form))
    (string (+ "\"" (string form) "\""))]
   [(or (instance? HyDict form) (instance? dict form))
    (+ "{" (.join " " (map -pprint form)) "}")]
   [(instance? list form)
    (+ "[" (.join " " (map -pprint form)) "]")]
   [(coll? form)
    (-pprint (list form))]
   [(cons? form)
    (+ "(" (-pprint (first form)) " . " (-pprint (rest form)) ")")]
   [True
    None]))

(defn hypprint [form &optional [outermost False]]
  (if outermost
    (list (map hypprint form))
    (print (-pprint form))))

(defn hypformat [form &optional [outermost False]]
  (if outermost
    (list (map hypformat form))
    (+ (-pprint form) "\n")))

(defmacro pretty/simplify [expr &rest args]
  `(hypprint (simplify '~expr ~@args)))

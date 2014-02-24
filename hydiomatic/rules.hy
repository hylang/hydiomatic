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

(import [hydiomatic.rulesets.arithmetico [*]]
        [hydiomatic.rulesets.quoteo [*]]
        [hydiomatic.rulesets.control-structo [*]]
        [hydiomatic.rulesets.equalityo [*]]
        [hydiomatic.rulesets.collectiono [*]]
        [hydiomatic.rulesets.syntaxo [*]]
        [hydiomatic.rulesets.optimo [*]]
        [hydiomatic.rulesets.warningso [*]]
        [adderall.dsl [*]])
(require adderall.dsl)
(require hydiomatic.macros)

(defn rules/default [expr q]
  (condᵉ
   [(rules/arithmeticᵒ expr q)]
   [(rules/quoteᵒ expr q)]
   [(rules/equalityᵒ expr q)]
   [(rules/control-structᵒ expr q)]
   [(rules/collectionᵒ expr q)]
   [(rules/syntaxᵒ expr q)]))

(defn rules/experimental [expr q]
  (condᵉ
   [(rules/default expr q)]
   [(rules/optimᵒ expr q)]))

(defn rules/warnings [expr q]
  (condᵉ
   [(rules/warningsᵒ expr q)]))

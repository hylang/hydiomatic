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
(import [hydiomatic.utils [prewalk]]
        [functools [partial]])
(require adderall.dsl)

(eval-and-compile
 (defn prep [freshes expr]
   (when (and (instance? HySymbol expr)
              (.startswith expr "?"))
     (.add freshes expr))
   expr)

 (defn rule [rule]
   (if (instance? HyExpression rule)
     `[~rule]
     (let [[[pat subst] rule]
           [freshes (set [])]]
       (prewalk (partial prep freshes) pat)
       (setv freshes (HyList freshes))
       `[(fresh ~freshes
                 (≡ expr ~pat)
                 (≡ out ~subst))]))))

(defmacro defrules [aliases &rest rules]
  `(defn-alias [~@aliases] [expr out]
     (condᵉ
      ~@(map rule rules))))

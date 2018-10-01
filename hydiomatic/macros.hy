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

(require [adderall.dsl [*]])
(require [hy.contrib.walk [let]])


(eval-and-compile
 (defn rule [rule]
   (if (instance? HyExpression rule)
     `[~rule]
     (let [pat (first rule)
           subst (second rule)]
         `[(prep (≡ expr ~pat)
                 (≡ out ~subst))]))))

(defmacro defrules [aliases &rest rules]
  `(do
     (require [adderall.internal [defn-alias]])
     (defn-alias [~@aliases] [expr out]
         (condᵉ
           ~@(map rule rules)))))

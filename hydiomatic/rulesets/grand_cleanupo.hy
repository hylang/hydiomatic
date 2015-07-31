;; hydiomatic -- The Hy Transformer
;; Copyright (C) 2015  Gergely Nagy <algernon@madhouse-project.org>
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
        [hy [HySymbol]])
(require adderall.dsl)
(require hydiomatic.macros)

(defn --transform-bindings/extend-symbol-with-nil-- [bindings]
  (setv new-bindings [])
  (for [binding bindings]
    (if (instance? HySymbol binding)
      (.append new-bindings [binding nil])
      (.append new-bindings binding)))
  new-bindings)

(defn simple-flatten [coll]
  (setv new-coll [])
  (for [member coll]
    (.extend new-coll member))
  new-coll)

(defrules [rules/grand-cleanupᵒ rules/grand-cleanupo]
  ;; (let [[x 1] [y 2] z] ...) => (let [x 1 y 2 z nil] ...)
  ;; (with [[x 1] [y 2] z] ...) => (with [x 1 y 2 z nil] ...)
  (prep
   (≡ expr `(~?op ~?bindings . ~?body))
   (memberᵒ ?op `[let with])
   (project [?bindings]
            (≡ ?new-bindings
               (--transform-bindings/extend-symbol-with-nil--
                ?bindings)))
   (project [?new-bindings]
            (≡ ?flat-bindings (simple-flatten ?new-bindings)))
   (condᵉ
    [(≡ ?op `let) (≡ ?new-op `$hydiomatic/let$)]
    [(≡ ?op `with) (≡ ?new-op `$hydiomatic/with$)])
   (≡ out `(~?new-op ~?flat-bindings . ~?body))))

(defrules [rules/grand-cleanup-finishᵒ rules/grand-cleanup-finisho]
  ;; $hydiomatic/let$ => let
  ;; $hydiomatic/with$ => with
  (prep
   (≡ expr `(~?op . ~?args))
   (memberᵒ ?op `[$hydiomatic/let$
                  $hydiomatic/with$])
   (condᵉ
    [(≡ ?op `$hydiomatic/let$)
     (≡ ?new-op `let)]
    [(≡ ?op `$hydiomatic/with$)
     (≡ ?new-op `with)])
   (≡ out `(~?new-op . ~?args))))

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

(import [adderall.dsl [*]]
        [adderall.extra.misc [*]]
        [hy [HyExpression HyString]])

(require [hy.contrib.walk [let]])
(require [adderall.dsl [*]])
(require [hydiomatic.macros [*]])

(defn --transform-bindings-- [bindings body]
  (let [new-bindings (lfor x bindings `(setv ~@x))]
    (+ new-bindings body)))

(defrules [rules/optimᵒ rules/optimo]
  ;; (defn foo [x] (let [[y (inc x)]] ...))
  ;;  => (defn foo [x] (setv y (inc x)) ...)
  (prep
   (memberᵒ ?op `[defn defun defn-alias defun-alias])
   (condᵉ
    [(≡ expr `(~?op ~?fname ~?params
                    ~(cons `let ?bindings ?body)))
     (≡ ?c (cons ?op ?fname ?params ?new-body))]
    [(≡ expr `(~?op ~?fname ~?params ~?docstring
                    ~(cons `let ?bindings ?body)))
     (typeᵒ ?docstring HyString)
     (≡ ?c (cons ?op ?fname ?params ?docstring ?new-body))])
   (project [?bindings ?body]
            (≡ ?new-body (--transform-bindings-- ?bindings ?body)))
   (project [?c]
            (≡ out (HyExpression ?c))))

  ;; (fn [x] (foo x)) => foo
  ;;  (for certain values of foo)
  (prep
   (condᵉ
    [(≡ expr `(~?f ~?xs ~(cons ?op ?xs)))]
    [(≡ expr `(~?f ~?xs ~?docstring ~(cons ?op ?xs)))])
   (memberᵒ ?f `[fn lambda])
   (condᵉ
    [(memberᵒ ?op `[and or not ~ del
                    quote
                    throw raise
                    = != < <= > >= is in is-not not-in
                    % / // ** << >>  | ^ &
                          + * -
                          += /= //= *= -= %= **= <<= >>= |= ^= &=])
     (≡ out expr)]
    (else (≡ out ?op)))))

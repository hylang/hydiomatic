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
        [adderall.extra.misc [*]]
        [hy [HyExpression HyList]])
(require adderall.dsl)
(require hydiomatic.macros)

(defrules [rules/syntaxᵒ rules/syntaxo]
  ;; (defn foo (x) ...) => (defn foo [x] ...)
  (prep
   (memberᵒ ?op `[defn defun defn-alias defun-alias])
   (≡ expr `(~?op ~?fname ~?params . ~?body))
   (typeᵒ ?params HyExpression)
   (project [?params]
            (≡ out `(~?op ~?fname ~(HyList ?params) . ~?body))))
  ;; (isinstance x klass) => (instance? klass x)
  [`(isinstance ~?x ~?klass) `(instance? ~?klass ~?x)]
  ;; (instance? float x) => (float? x)
  [`(instance? float ~?x) `(float? ~?x)]
  ;; (instance? int x) => (integer? x)
  [`(instance? int ~?x) `(integer? ~?x)]
  ;; (instance? str x) => (string? x)
  [`(instance? str ~?x) `(string? ~?x)]
  ;; (instance? unicode x) => (string? x)
  [`(instance? unicode ~?x) `(string? ~?x)]
  ;; (for* [x iteratable] (yield x))
  ;;  => (yield-from iteratable)
  [`(for* [~?x ~?iteratable] (yield ~?x))
   `(yield-from ~?iteratable)]
  ;; (-> a) => a
  [`(-> ~?a) ?a]
  ;; (-> (-> x) y) => (-> x y)
  (prep
   (≡ expr `(-> ~?inner . ~?y))
   (≡ ?inner `(-> . ~?x))
   (≡ ?o `(-> . ~?x))
   (typeᵒ ?inner HyExpression)
   (typeᵒ ?x HyExpression)
   (typeᵒ ?y HyExpression)
   (appendᵒ ?o ?y out))
  ;; (kwapply (.foo bar baz) {...}) => (apply bar.foo [baz] {...})
  (prep
   (≡ expr `(kwapply ~?target ~?kwargs))
   (typeᵒ ?target HyExpression)
   (firstᵒ ?target ?method)
   (project [?method]
            (≡ true (.startswith ?method "."))
            (fresh [?m ?o]
                   (≡ ?target `(~?m ~?o . ~?params))
                   (typeᵒ ?o HySymbol)
                   (project [?params ?m ?o]
                            (≡ ?new-params (HyList ?params))
                            (≡ ?call-name (+ ?o ?m)))))
   (≡ out `(apply ~?call-name ~?new-params ~?kwargs)))
  ;; (kwapply (foo bar baz) {...} => (apply foo [bar baz] {...})
  (prep
   (≡ expr `(kwapply (~?method . ~?params) ~?kwargs))
   (project [?params]
            (≡ ?new-params (HyList ?params)))
   (≡ out `(apply ~?method ~?new-params ~?kwargs))))

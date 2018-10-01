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
        [hy [HyExpression HyList]])

(require [adderall.dsl [*]])
(require [hydiomatic.macros [*]])

(defrules [rules/syntaxᵒ rules/syntaxo]
  ;; (defn foo (x) ...) => (defn foo [x] ...)
  (prep
   (memberᵒ ?op `[defn defun defn-alias defun-alias])
   (≡ expr (cons ?op ?fname ?params ?body))
   (typeᵒ ?params HyExpression)
   (project [?params]
            (≡ out (cons ?op ?fname (HyList ?params) ?body))))

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
   (≡ expr (cons `-> ?inner ?y))
   (≡ ?inner (cons `-> ?x))
   (≡ ?o (cons `-> ?x))
   ;; TODO: This is cumbersome; is there a better expectation for the desired
   ;; output type of `cons?` Should we change `cons` to always output a
   ;; HyExpression?
   (condᵉ [(typeᵒ ?inner HyExpression)]
          [(typeᵒ ?inner list)])
   (condᵉ [(typeᵒ ?x HyExpression)]
          [(typeᵒ ?x list)])
   (condᵉ [(typeᵒ ?y HyExpression)]
          [(typeᵒ ?y list)])
   (appendᵒ ?o ?y out))

  ;; (kwapply (.foo bar baz) {...}) => (apply bar.foo [baz] {...})
  (prep
   (≡ expr `(kwapply ~?target ~?kwargs))
   (typeᵒ ?target HyExpression)
   (firstᵒ ?target ?method)
   (project [?method]
            (≡ True (.startswith ?method "."))
            (fresh [?m ?o]
                   (≡ ?target (cons ?m ?o ?params))
                   (typeᵒ ?o HySymbol)
                   (project [?params ?m ?o]
                            (≡ ?new-params (HyList ?params))
                            (≡ ?call-name (+ ?o ?m)))))
   (≡ out `(apply ~?call-name ~?new-params ~?kwargs)))

  ;; (kwapply (foo bar baz) {...} => (apply foo [bar baz] {...})
  (prep
   (≡ expr `(kwapply ~(cons ?method ?params) ~?kwargs))
   (project [?params]
            (≡ ?new-params (HyList ?params)))
   (≡ out `(apply ~?method ~?new-params ~?kwargs))))

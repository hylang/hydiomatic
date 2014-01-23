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
(require adderall.dsl)

(defn-alias [rules/control-structᵒ rules/control-structo] [expr out]
  (condᵉ
   ;; (if test y nil) => (when test y)
   [(fresh [test yes-branch]
           (≡ expr `(if ~test ~yes-branch nil))
           (≡ out `(when ~test ~yes-branch)))]
   ;; (if test nil n) => (unless test n)
   [(fresh [test no-branch]
           (≡ expr `(if ~test nil ~no-branch))
           (≡ out `(unless ~test ~no-branch)))]
   ;; (if (not test) a b) => (if-not test a b)
   [(fresh [test branches]
           (≡ expr `(if (not ~test) . ~branches))
           (≡ out `(if-not ~test . ~branches)))]
   ;; (if test (do y)) => (when test y)
   [(fresh [test y]
           (≡ expr `(if ~test (do . ~y)))
           (≡ out `(when ~test . ~y)))]
   ;; (when (not test) stuff) => (unless test stuff)
   [(fresh [test body]
           (≡ expr `(when (not ~test) . ~body))
           (≡ out `(unless ~test . ~body)))]
   ;; (do x) => x
   [(≡ expr `(do ~out))]
   ;; (when test (do x)) => (when test x)
   ;; (unless test (do x)) => (unless test x)
   [(fresh [op test body]
           (memberᵒ op `[when unless])
           (≡ expr `(~op ~test (do . ~body)))
           (≡ out `(~op ~test . ~body)))]
   ;; (if test a) => (when test a)
   ;; (if-not test a) => (unless test a)
   [(fresh [op new-op test branch]
           (condᵉ
            [(≡ op 'if) (≡ new-op 'when)]
            [(≡ op 'if-not) (≡ new-op 'unless)])
           (≡ expr `(~op ~test ~branch))
           (≡ out `(~new-op ~test ~branch)))]))

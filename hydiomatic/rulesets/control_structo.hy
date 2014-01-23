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
(require hydiomatic.macros)

(defrules [rules/control-structᵒ rules/control-structo]
  ;; (if test y nil) => (when test y)
  [[test yes-branch]
   `(if ~test ~yes-branch nil)
   `(when ~test ~yes-branch)]
  ;; (if test nil n) => (unless test n)
  [[test no-branch]
   `(if ~test nil ~no-branch)
   `(unless ~test ~no-branch)]
  ;; (if (not test) a b) => (if-not test a b)
  [[test branches]
   `(if (not ~test) . ~branches)
   `(if-not ~test . ~branches)]
  ;; (if test (do y)) => (when test y)
  [[test y]
   `(if ~test (do . ~y))
   `(when ~test . ~y)]
  ;; (when (not test) stuff) => (unless test stuff)
  [[test body]
   `(when (not ~test) . ~body)
   `(unless ~test . ~body)]
  ;; (do x) => x
  [[body] `(do ~body) body]
  ;; (when test (do x)) => (when test x)
  [[test body]
   `(when ~test (do . ~body))
   `(when ~test . ~body)]
  ;; (unless test (do x)) => (unless test x)
  [[test body]
   `(unless ~test (do . ~body))
   `(unless ~test . ~body)]
  ;; (if test a) => (when test a)
  [[test branch]
   `(if ~test ~branch)
   `(when ~test ~branch)]
  ;; (if-not test a) => (unless test a)
  [[test branch]
   `(if-not ~test ~branch)
   `(unless ~test ~branch)])

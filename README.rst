Granjure 
================================================================================

Functor, Applicative, Arrow, and Monad.

Examples
--------------------------------------------------------------------------------

Applicative
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: clojure

  (require 'granjure.data.function)
  (use '[granjure.control.applicative :only [<*> *> <* <**>]])

  (<*> list inc 42)  ; (42 43)
  (<* list inc 42)   ; (42)
  (*> list inc 42)   ; 42
  (<**> inc list 42) ; (42 43)

Arrow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: clojure

  (require 'granjure.data.function)
  (use '[granjure.control.arrow :only [fst snd &&& ***]])

  (fst inc [42 42])     ; (43 42)
  (snd dec [42 42])     ; (42 41)
  (&&& inc dec 42)      ; (43 41)
  (*** inc dec [42 42]) ; (43 41)

Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: clojure

  (require 'granjure.data.list)
  (use '[granjure.control.monad :only [return >> >>=]])

  (def list1 [1 2 3])
  (def list2 [4 5 6])

  ; First, we show ugly deep-nested lambda monad.
  (>>= list1 (fn [n]
    (>>= list2 (fn [m]
      (return (list n m))))))
  ; ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))

  ; But you can use brief and flat syntax. that is the do-notation and the monad-comprehension-notation.
  (use '[granjure.control.monad :only [do-m]])
  (do-m (
    n <- list1 :.
    m <- list2 :in
    (return (list n m))))

  (use '[granjure.control.monad.zip :only [do-mzip]])
  (do-mzip ((list n m) | n <- list1 :. m <- list2))

Features
--------------------------------------------------------------------------------

Curried functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All functions of Granjure are curried.

.. code:: clojure

  (use '[granjure.data.tuple :only [tuple2]])
  (tuple2 1)     ; #<primitive$uncurry$fn__1193 granjure.primitive$uncurry$fn__1193@32b3869c>
  ((tuple2 1) 2) ; (1 2)
  (tuple2 1 2)   ; (1 2)


Infix-notation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each library of Granjure provides infix-notation macros only for syntax-suger.
You can use infix-notation if you need, by the `infixing library <https://github.com/pasberth/infixing/>` .

.. code:: clojure

  (use '[granjure.control.applicative :only [<*> applicative-rule]])
  (use '[infixing.core :only [infixing]])

  (defmacro ado [code] (infixing applicative-rule code))
  (def f (ado (list <*> inc)))
  (f 42) ; (42 43)


Arrows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: clojure

  (require 'granjure.data.function)
  (use '[granjure.control.arrow :only [fst snd &&& ***]])

  (fst inc [42 42])     ; (43 42)
  (snd dec [42 42])     ; (42 41)
  (&&& inc dec 42)      ; (43 41)
  (*** inc dec [42 42]) ; (43 41)

**ArrowChoice**

.. code:: clojure

  (require 'granjure.data.function)
  (require 'granjure.data.either)
  (import '[granjure.data.either Left Right])
  (use '[granjure.control.arrow.choice :only [left right +++ |||]])

  (left dec (Left. 42))     ; (Left 41)
  (right inc (Left. 42))    ; (Left 42)
  (+++ dec inc (Left. 42))  ; (Left 41)
  (+++ dec inc (Right. 42)) ; (Right 43)
  (||| dec inc (Left. 42))  ; 41
  (||| dec inc (Right. 42)) ; 42

More
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO.
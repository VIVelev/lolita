# A naive LISP interpreted by a tower of interpreters, compiled to C.

lolita is a zero-dependency (using only the 'base' package) implementation of a LISP with full support of macros
that compiles to C. Of interest in the implementation is the way macros are achieved - lazily building an infinite
tower of interpreters.

## A Tower of Interpreters

Consider the `unless` macro
```scheme
(begin
  (defmacro (unless cond body)
    `(if ,cond 'passthrough ,body))

  (unless #t (+ 2 2)))
```
`defmacro` is a special keyword that defines new keywords. In this case,
the keyword `unless` is defined. Once a new keyword is defined using
`defmacro`, upon occurrence of the keyword the macro is "expanded"
by evaluating the body of the macro in a new, meta-level, interpreter.

If the body of the macro itself contains macro definitions and invocations,
those macros are in turn expanded, thus, a new meta-meta-level interpreter is created.
The process is repeated as many times as necessary.

![tower of interpreters diagram](./assets/tower.svg)

## Compilation to C

Code is compiled to C via the supporting `scheme.h` runtime. The output C code is expressed in C macros
defined in `scheme.h` for simplicity and readability. The strategy adopted preserves the flavour of an
expression-centered language like LISP as much as possible.

### How to compile?

To compile, for example, `factorial.scm` run:
```bash
cabal run lolita -- factorial.scm
```
This will output a `out.c` file which can then be compiled and ran:
```bash
cc out.c && ./a.out
```
## Source files

- `src/`
  - `Main.hs` - the command line interface
  - `MonadT.hs` - simple monad transformers library ala [transformers](https://hackage.haskell.org/package/transformers)
  - `Parse.hs` - LISP reader implemented with combinators
  - `Objectify.hs` - AST builder and macro expansion
  - `Walk.hs` - various code walkers for processing the AST
  - `Codegen.hs` - performs the final step of converting the processed AST to C code.
- `scheme.h` - the C runtime required to execute the output C code

## Leftovers and limitations (things I meant to do, but didn't end up doing)

It should be relatively straightforward to implement mutation in the language via `set!`. An appropriate API
should be added to `scheme.h` and a code walker to label mutable variables.

Currently, there is no garbage collector whatsoever; however, a [Boehm](https://en.wikipedia.org/wiki/Boehm_garbage_collector) style
garbage collector should be relatively straightforward to adopt.

## Credits

Various sources have been referenced throughout the codebase; however, one source has been paramount
in the development of lolita: _Lisp in Small Pieces_ by Christian Queinnec.

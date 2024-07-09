# TODOs

## Jun 30, 2024

 - [x] Make `ErrorT String (ReaderT Env Identity) Value` possible
   - [x] This would require abstracting away a `MonadReader` which
     `ErrorT` needs to implement.

## Jul 1, 2024

 - [x] Implement parser combinators from scratch.
 - [x] Should parse basic pairs, atoms, nulls.

## Jul 2, 2024
 - [x] Add Monadic State to parser
 - [x] Improve errors (i.e. display the row/col where the error occurred)
 - [x] Parse more complicated structures (i.e. lists)
   - [x] Figure out an efficient way to parse lists.
     - Ideally we don't rewrite `(a b c)` to `(a . (b . (c . '())))`
 - [x] Test the parser

## Jul 3, 2024
 - [x] Parse atomic literals

## Jul 4, 2024
 - [x] Introduce the basic AST structure, this will look like the objectification
   algorithm from LiSP.
     - [x] The question here is, how to model the inheritance structure? Solve that!
 - [x] Make a simple objectifier.
 - [x] Structure an environment
 - [x] Using the objectifier, make a simple evaluator

## Jul 5, 2024
 - [x] A simple version of `defmacro`
 - [x] Refactor
   - [x] Is there a way to combine `ReaderT` and `StateT` nicely for the objectifier?
 - [x] Pretty print `SExp`
 - [x] Add read script from file functionality

## Jul 6, 2024
 - [x] It's hight time I support backquotes.
 - [x] Test defmacro

## Jul 8, 2024
 - [x] Handle function applications
 - [x] Add assignments
 - [x] Code walking
   - [x] Boxer (read, write, create)

## Jul 9, 2024
   - [ ] Lambda lifting

## Long term

 - [ ] Should I add `MonadTrans` style `lift`s?
 - [ ] How to implement exception handling in the language itself?
   Something like the handler demonstrated in LiSP (via stack) won't
   do it, or will it?

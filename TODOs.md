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
 - [ ] Introduce the basic AST structure, this will look like the objectification
   algorithm from LiSP.
     - [ ] The question here is, how to model the inheritance structure? Solve that!
 - [ ] Make a simple objectifier.
 - [ ] Structure an environment
 - [ ] Using the objectifier, make a simple evaluator

## Jul 4, 2024

 - [ ] Add assignments

## Long term

 - [ ] Should I add `MonadTrans` style `lift`s?
 - [ ] How to implement exception handling in the language itself?
   Something like the handler demonstrated in LiSP (via stack) won't
   do it, or will it?

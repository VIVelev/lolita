 = TODOs

 == Jun 30, 2024

 - [x] Make `ErrorT String (ReaderT Env Identity) Value` possible
   - [x] This would require abstracting away a `MonadReader` which
     `ErrorT` needs to implement.

== Jul 1, 2024

 - [x] Implement parser combinators from scratch.
 - [x] Should parse basic pairs, atoms, nulls.

== Jul 2, 2024
 - [x] Add Monadic State to parser
 - [ ] Improve errors (i.e. display the row/col where the error occurred)
 - [ ] Parse more complicated structures (i.e. lists)
   - [ ] Figure out an efficient way to parse lists.
     - Ideally we don't rewrite `(a b c)` to `(a . (b . (c . '())))`
 - [ ] Introduce the basic AST structure, this will look like the objectification
   algorithm from LiSP.
     - [ ] The question here is, how to model the inheritance structure? Solve that!

== Jul 3, 2024

 - [ ] Handle IO, What is `liftIO`?
 - [ ] Should I add `MonadTrans` style `lift`s?

== Long term

 - [ ] How to implement exception handling in the language itself?
   Something like the handler demonstrated in LiSP (via stack) won't
   do it, or will it?

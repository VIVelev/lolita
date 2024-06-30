 - [ ] Make `ErrorT String (ReaderT Env Identity) Value` possible
   - [ ] This would require abstracting away a `MonadReader` which
     `ErrorT` needs to implement.

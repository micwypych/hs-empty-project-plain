# hs-empty-project-plain

Początkowy setup projektu (wysatrczy wykonać raz):
```bash
cabal sandbox init
cabal update
cabal install --only-dependencies
```

Kompilacja projektu:
```bash
cabal build
```

Kompilacja kodu i załadowanie biblioteki do ghci:
```bash
cabal repl
```

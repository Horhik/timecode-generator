# timecode-generator 
A simple Haskell script for creating timecodes by tracking changes in remote Markdown file

**Requimpments**
- Nix package manager installed
## Installing
Clone repository and run `nix-build -A pkgs.haskell.packages.ghc8107.timecode-generator` inside directory

You'll find binary in `./result/bin/timecode-generator`
#### In nix-shell
run `nix-shell` and then compile it with cabal inside (`cabal build timecode-generator`)

## how to use
`./timecode-generator <URL to fetch> <Timecode filename>`

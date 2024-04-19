# Advent of Code

These are my Advent of Code attempts. So far, I have used AoC as an opportunity to write elegant code in fun languages.

## 2022

I attempted AoC 2022 in Haskell. Given my math background, it was a delight.

I think I was using GHC 9.2.5. The file structure is a mess (e.g. full of duplicated and modified files) and no Nix development shell is provided because I was trying to focus on the Haskell at the time. I may fix this in the near future. For some of the later solutions, I gave up trying to avoid dependencies and used `megaparsec`.


## 2023

I attempted AoC 2023 in (Guile) Scheme, before switching to Clojure due to some frustrations. The file structure is much nicer this time --- I learned from the previous year's attempt. It includes a `flake.nix` development shell.

Less fun than Haskell in my opinion, but still quite educational and entertaining. More details can be found in [2023's readme](2023/README.md). There is also a [rant/article about the Day 8 problem](2023/day8_ARTICLE.md).

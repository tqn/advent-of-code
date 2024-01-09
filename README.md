# Advent of Code 2023

I decided to use this year's Advent of Code to learn Scheme. I chose to use the Guile implementation and tried use SRFIs and not Guile modules whenever possible.

There is an extensive write-up and description of an algorithm in the [day 8 readme](src/day8/README.md), which I may one day turn into a blog post.

## Running

Activate the nix development shell via direnv, or manually with `nix develop` in the root directory. (You can also run `nix develop .#clojure` or `nix develop .#guile`. The default development shell has both.)

To run a Guile program, say `day1`, run `guile -s src/day1.scm data/day1.txt`.

To run a Clojure program, say `day9`, run `clj -M -m day9 data/day9.txt`.

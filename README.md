# Advent of Code 2023

I decided to use this year's Advent of Code to learn Scheme. I chose to use the Guile implementation and tried use SRFIs and not Guile modules whenever possible.

There is an extensive write-up and description of an algorithm in the [day 8 readme](src/day8/README.md), which I may one day turn into a blog post.

## Running

Activate the nix development shell via direnv, or manually with `nix develop` in the root directory.

To run a Guile program, say `day1`, `cd` into `day1` and run `guile -s day1.scm day1.txt`.

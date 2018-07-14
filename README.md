# hedgehog-checkers

[![Build Status](https://travis-ci.org/bitemyapp/hedgehog-checkers.svg?branch=master)](https://travis-ci.org/bitemyapp/hedgehog-checkers)

`hedgehog-checkers` wraps up the expected properties associated with various standard type classes as Hedgehog properties. Inspired by Conal Elliot's [checkers](https://hackage.haskell.org/package/checkers) library.

`hedgehog-checkers-lens` depends on `hedgehog-checkers` and includes `Lens`, `Prism`, `Setter`, `Traversal`, and `Iso`. The properties are defined for the widely used [lens library](https://github.com/ekmett/lens/).

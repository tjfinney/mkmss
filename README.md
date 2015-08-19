# mkmss
## Tim Finney 2015

## Introduction

mkmss is a computer program which simulates production of a textual corpus comprised of multiple copies of an initial text. The initial text is represented as a sequence of some number of ones.

`1111111111`

Each 1 represents the initial state of a phrase of the initial text. Whenever a copy is made, the states of some phrases may change so a copy of the initial text might look like this:

`1112111211`

And a copy of the copy might look like this:

`1113111212`

The program is stochastic, using random number generation to drive its various simulation processes. How each process behaves is determined by a corresponding model which is in turn constrained by a set of user inputs. Hopefully the models used in this program successfully emulate the real world processes they are designed to mimic, and avoid unnecessary complexity. A good match between data produced by the simulation and textual variation data from a real corpus would encourage belief that the models used here are adequate.

## Installing mkmss

mkmss uses the [R][n1] language and environment for statistical computing and graphics. It can run as a standalone program (see [helpers.R](helpers.R)) or be used in conjunction with [RStudio] and the [Shiny] package.

[n1]: https://www.r-project.org/        "R statistics"

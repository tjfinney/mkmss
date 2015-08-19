# mkmss
## Tim Finney 2015

## Introduction

mkmss is a computer program which simulates development of a textual corpus through incremental changes introduced as the text is propagated through manual copying. The simulated corpus is comprised of computer-generated copies of an initial text. The initial text is represented as a sequence of some number of ones.

`1111111111`

Each 1 represents the initial state of a phrase of the initial text. Whenever a copy is made, the states of some phrases may change so a copy of the initial text might look like this:

`1112111211`

A copy of the copy might look like this:

`1113111212`

The program is stochastic, using random number generation to drive its various simulation processes. How each process behaves is determined by a corresponding model which is in turn constrained by user inputs. Hopefully the models used in this program successfully emulate the real world processes they are designed to mimic, and avoid unnecessary complexity. A good match between data produced by the simulation and textual variation data from a real corpus would encourage belief that the models used here are adequate.

## Installing mkmss

mkmss uses the [R](https://www.r-project.org/) language and environment for statistical computing and graphics. It can run as a standalone program (using [helpers.R](helpers.R)) but also has a graphical user interface for those who would like to interact with the program without having to edit the source code. This interface uses the [Shiny](http://shiny.rstudio.com/) package developed by [RStudio](https://www.rstudio.com/), which is free (and extremely good) software. editing helpers.R, experiment with the program while avoiding Alternatively a graphical user interface based on RStudio's Shiny package can be used to change user inputs and observe the effects of these changes. The RStudio latter mode is recommended for those interested in experimenting with the simulation or be used in conjunction with [RStudio] and the [Shiny] package.

[n1]:         "R statistics"

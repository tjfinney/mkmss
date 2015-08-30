# mkmss

## Tim Finney 2015

## Introduction

The `mkmss` computer program simulates incremental development of a textual corpus as changes are introduced to instances of the text as they are propagated through multiple copying events. The simulated corpus is comprised of computer-generated copies descended from an initial text which is represented by a sequence of some number of ones.

`1111111111`

Each `1` represents the initial state of a *character* of the initial text. The broadest meaning of *character* is "something that can vary" and might include differences of orthography (i.e. those relating to spelling, diacritics, punctuation). However, semantic variations (i.e. those that affect meaning) are typically uppermost in the minds of researchers. 

Whenever the program makes a copy, the states of zero or more characters are subject to change. A copy of the initial text might therefore look like this:

`1112111211`

A copy of the copy might look like this:

`1113111212`

The program is stochastic, using random number generation to drive its various simulation processes. How each process behaves is determined by a corresponding model which is in turn constrained by user inputs. Hopefully the models used in this program successfully emulate the real world processes they are designed to mimic while avoiding unnecessary complexity. A good match between data produced by the simulation and textual variation data from a real corpus would encourage belief that the models used here are sufficient.

## Installing mkmss

The `mkmss` program uses the [R](https://www.r-project.org/) language and environment for statistical computing and graphics, which is free software. The simulation can run as a standalone program (using [helpers.R](helpers.R)) but also has a graphical user interface so that users can interact with the program without having to edit the source code. The interface uses the [Shiny](http://shiny.rstudio.com/) package developed by [RStudio](https://www.rstudio.com/), which is also free software.

It is essential to install R on the user's computer in order to run any part of the simulation. Instructions on how to download and install R are located [here](https://www.r-project.org/)

editing helpers.R, experiment with the program while avoiding Alternatively a graphical user interface based on RStudio's Shiny package can be used to change user inputs and observe the effects of these changes. The RStudio latter mode is recommended for those interested in experimenting with the simulation or be used in conjunction with [RStudio] and the [Shiny] package.



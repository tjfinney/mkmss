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

## Downloading, installating, and running mkmss

The `mkmss` program uses the [R](https://www.r-project.org/) language and environment for statistical computing and graphics. While the simulation can run using R alone, many will prefer to interact with `mkmss` through a graphical interface provided by the [RStudio](https://www.rstudio.com/products/rstudio/) integrated development environment (IDE) and RStudio's [Shiny](http://shiny.rstudio.com/) package. R, Rstudio, and Shiny are all free software.

R can be downloaded from one of the sites listed [here](https://cran.r-project.org/mirrors.html); The RStudio desktop edition is available [here](https://www.rstudio.com/products/rstudio/#Desktop); Shiny is installed by starting R then typing the following at the R command prompt. (An Internet connection is required.)

`install.packages("shiny")`

`mkmss` uses a number of other R packages which need to be installed as well.

`install.packages("graphics")`
`install.packages("cluster")`
`install.packages("ape")`

The `mkmss` program itself is installed as follows:

1. Create a directory to hold the program components. (For example, you could make a folder called "mkmss" on your desktop.)
2. Download [server.R](server.R), [ui.R](ui.R), and [helpers.R](helpers.R) to the directory created at step 1.

To launch `mkmss` with the graphical interface:

1. Start RStudio.
2. Open server.R or ui.R within RStudio.
3. Press the `Run app` button.

To launch `mkmss` as a standalone program:

1. Open `helpers.R` with an editor (such as RStudio) then change the "2" to a "1" in the following line located near the end of the file: `if (c(TRUE, FALSE)[2]) {`.
2. Edit `helpers.R` to set desired input parameters immediately below the `if (c(TRUE, FALSE)[2]) {` line.
3. At the R command prompt, set the R working directory to the directory where `helpers.R` is located. E.g. `setwd("~/Desktop/mkmss")`.
4. Run `helpers.R` by typing `source("helpers.R")` at the R command prompt.

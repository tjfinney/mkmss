# mkmss

## Tim Finney, 2015

## Introduction

The `mkmss` computer program simulates incremental development of a textual corpus as changes are introduced to instances of the text as they are propagated through multiple copying events. The simulated corpus is comprised of computer-generated copies descended from an initial text which is represented by a sequence of some number of ones.

`1111111111`

Each `1` represents the initial state of a *character* of the initial text. The broadest meaning of *character* is "something that can vary" and might include differences of orthography (i.e. those relating to spelling, diacritics, punctuation). However, semantic variations (i.e. those that affect meaning) are typically uppermost in the minds of researchers. 

Whenever the program makes a copy, the states of zero or more characters are subject to change. A copy of the initial text might therefore look like this:

`1112111211`

A copy of the copy might look like this:

`1113111212`

An explanation of user inputs and program components is planned to be provided in the `mkmss` [wiki](https://github.com/tjfinney/mkmss/wiki) pages.

## Download, install, and run mkmss

The `mkmss` program uses the [R](https://www.r-project.org/) language and environment for statistical computing and graphics. While the simulation can run using R alone, many will prefer to interact with `mkmss` through a graphical interface provided by the [RStudio](https://www.rstudio.com/products/rstudio/) integrated development environment (IDE) and RStudio's [Shiny](http://shiny.rstudio.com/) package. R, Rstudio, and Shiny are all free software.

R can be downloaded from one of the sites listed [here](https://cran.r-project.org/mirrors.html); the RStudio desktop edition is available [here](https://www.rstudio.com/products/rstudio/#Desktop); Shiny is installed by starting R then typing the following at the R command prompt. (An Internet connection is required.)

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
2. Open `server.R` or `ui.R` within RStudio.
3. Press the `Run app` button.

To launch `mkmss` as a standalone program:

1. Enable output from the standalone component `helpers.R` by opening the file with an editor (such as RStudio) then changing the "2" to a "1" in the following line located near the end of the file: `if (c(TRUE, FALSE)[2]) {`.
2. Edit the input parameters immediately below the `if (c(TRUE, FALSE)[2]) {` line to have whatever values are desired.
3. At the R command prompt set the R working directory to the directory where `helpers.R` is located. On Mac and Linux systems this would be achieved by typing `setwd("~/Desktop/mkmss")` if `~/Desktop/mkmss` were the path to the directory where `helpers.R` is installed. On other systems the path syntax may differ; e.g. on Windows something like `C:\Desktop\mkmss` would be required.
4. Run `helpers.R` by typing `source("helpers.R")` at the R command prompt.

When output is enabled for the `helpers.R` standalone component it saves the data and distance matrices for recovered texts as comma separated vector files named `mkmss.data.txt` and `mkmss.dist.txt`. These are saved in the R working directory, and can be opened for inspection using a spreadsheet program or text editor.

If output is enabled then the entire domain object is also made available as a variable named `world`. This object contains every item produced during a run of the simulation program, and is potentially large. Consequently it is not a good idea to attempt to view the `world` object directly (e.g. by typing `world` at the command prompt). Components of the `world` object may be inspected using `$` notation at the command prompt; a few examples are listed below:

* `world$nn.stt` List numbers of states for all characters used in the current run of the simulation.
* `world$pll[["Rome"]]$stt` List of preferred states for each character for the `Place` object named *Rome*. (By default `helpers.R` has `Place` objects for Rome, Ephesus, Antioch, and Alexandria.)
* `world$pll[["Rome"]]$tt.extant[[1]]` First `Text` object in the collection of extant texts for Rome.
* `world$pll[["Rome"]]$tt.lost[[1]]` First `Text` object in the collection of lost texts for Rome.
* `world$pll[["Rome"]]$tt.lost[[1]]$events` List of events for the first `Text` object in the collection of lost texts for Rome.

(The final step of the simulation process recovers a number of texts from the extant and lost collections of each `Place`.)

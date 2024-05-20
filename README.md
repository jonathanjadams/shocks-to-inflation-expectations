# Shocks to Inflation Expectations: Replication Code

This repository contains the replication package for the following paper:

* Jonathan J Adams and Philip Barrett "Shocks to Inflation Expectations",  *Review of Economic Dynamics*, (2024).

## Required Software and Packages

Estimation exercises were carried out in `R`.  Simulations were carried out in `Matlab` and analyzed in `R`.  All results are confirmed on `Windows` using `Matlab` version `2019a`, `R` version 4.1.

A full list of the `R` packages needed to replicate our results is as follows:

  * `BigVAR`
  * `expm`
  * `forecast`
  * `ggplot2`
  * `lpirfs` 
  * `lubridate`
  * `magrittr`
  * `readxl`
  * `tidyverse`
  * `vars`
  * `xtable`
  * `zoo`

The Matlab replication requires the following toolkits (which are included in the `Toolkits` folder):

  * `Uhlig Toolkit` for solving DSGE models from [Harald Uhlig](https://home.uchicago.edu/~huhlig/js/Toolkit_4.3.zip "Uhlig")
  * `BEET Toolkit` for solving models without rational expectations ([documented here](https://github.com/jonathanjadams/BEET))

## Replication Code Order and Run-times

Paper tables and figures can be replicated by running codes in the following order:

1. `NK_model_static` (10 seconds)
2. `NK_model_dynamics` (10 seconds)
3. `code/sessions/master.R` (~10 hours) NB: Need to change the root directory to refer to local file structure (see "Empirical Analysis" below).

## Models

Code for our simulations appears in the directory `models` which
contains two files:

  * `NK_model_static` contains code to replicate the illustrative "AD-AS" figures in Section 2 and Appendix B.
  * `NK_model_dynamic` contains code to solve the dynamic model appearing in Section 6 of the paper, producing both impulse response functions and the Monte Carlo simulated dataset.  Set seed 0 to reproduce the paper's results exactly.  Be sure to add the toolkit folders to your path.
  
To reproduce the Monte Carlo exercises, first simulate the model using `NK_model_dynamic`, which recreates the files stored in `data/simulated
/multiple_shocks`.  Then procede with the `R` code.

## Empirical Analysis

Datasets are in the folder `data`

To reproduce the figures and tables in the paper, you should follow these steps
 1. Open `code/sessions/master.R`
 2. Edit line 16 so that root.dir refers to the location of this file on this system
 3. Run `code/sessions/master.R`. This takes a very long time, although should complete if left to run overnight on a modern laptop.
 4. The figures and tables in the paper are saved in folder called "output".
 5. To reset the outputs from the replication package, delete all the files and folders in: `graphs`, `tables`, `output`, and `model_solutions`. You can also delete the file `data/baseline_time_series.rdata`


Here is a short oveview of master.R script. Numbers correspond to the sections used in the code

 0. Set up. The bulk of this is in defining the various different versions of the model used, which are described in the variable l.cases.  Sections 1-8 of the code then are contained within the main loop which iterates over these versions.
 1. Unpacks assorted details for the current model run.
 2. Creates the dataset used in the current model run.
 3. Estimates the reduced form of the VAR.
 4. Converts reduced form to structural form. Adds the local projection if required.
 5. Makes the IRFs and variance decompositions by bootstrap.
 6. Robustness checks. The rolling-window estimation takes a very long time.  If you want this to go faster, you can set n.boot in section 0 to a smaller number, eg. 100, and it will only take a couple of hours although the bootstrap will be less accurate.
 7.  Makes standard charts for this run.
 8.  Saves the current model run.
 9.  Tidies up. Makes charts which compare across runs, runs the model on the simulated data, and produces the charts/tables using narrative shocks. Finally, this copies all the figures used in the paper to the output folder and renames them for convenience.


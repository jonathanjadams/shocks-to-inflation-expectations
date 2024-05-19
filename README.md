# Shocks to Inflation Expectations: Replication Code

This repository contains the replication package for the following paper:

* Jonathan J Adams and Philip Barrett "Shocks to Inflation Expectations",  *Review of Economic Dynamics*, (2024).

## Required Software and Packages

Estimation exercises were carried out in `R`.  Simulations were carried out in `Matlab` and analyzed in `R`.

A full list of the `R` packages needed to replicate our results is as follows:

  * `ggplot`
  * `etc.`
  * `etc.`
  * `etc.`

The Matlab replication was confirmed on version `2019a`, and also requires the following toolkits (which are included in the `Toolkits` folder):

  * `Uhlig Toolkit` for solving DSGE models from [Harald Uhlig](https://home.uchicago.edu/~huhlig/js/Toolkit_4.3.zip "Uhlig")
  * `BEET Toolkit` for solving models without rational expectations ([documented here](https://github.com/jonathanjadams/BEET))

## Replication Code Order and Run-times

Paper tables and figures can be replicated by running codes in the following order:

1. `NK_model_static` (10 seconds)
2. `NK_model_dynamics` (10 seconds)
3. `more here`...

## Models

Code for our simulations appears in the directory `models` which
contains two files:

  * `NK_model_static` contains code to replicate the illustrative "AD-AS" figures in Section 2 and Appendix B.
  * `NK_model_dynamic` contains code to solve the dynamic model appearing in Section 6 of the paper, producing both impulse response functions and the Monte Carlo simulated dataset.  Set seed 0 to reproduce the paper's results exactly.
  * `MONTE CARLO R CODE NAME` contains code to replicate ...
  
To reproduce the Monte Carlo exercises, first simulate the model using `NK_model_dynamic`, then...

## Empirical Analysis


Datasets are somewhere.  Code is also somewhere.

#' ---
#' author: 'Jan Vanhove'
#' title: 'Rerandomisation testing with blocking'
#' date: '2025/07/30'
#' output: 
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    number_sections: true
#'    theme: sandstone
#'    highlight: tango
#'    dev: svg
#'    df_print: paged
#' ---
#' 
#' ```{r setup, include=FALSE}
#' knitr::opts_chunk$set(message = FALSE)
#' ```

#' # Preliminaries
#' The file `functions/rerandomisation_pvals.R` contains a few
#' R functions that can be used to compute rerandomisation _p_-values
#' in experiments where participants are randomly assigned to one of two
#' conditions.
#' 
#' Load these functions like so:
library(here)
source(here("functions", "rerandomisation_pvals.R"))
#' We'll also use the `tidyverse` for a visualisation.
library(tidyverse)

#' # Exhaustive rerandomisation with blocking
#' Read in a made-up dataset with 32 participants (16 blocks of two participants each):
d <- read_csv(here("datasets", "example_blocking.csv"))
#' Here's one quick visualisation of the data that also shows the blocking structure:
ggplot(d,
       aes(x = Score, y = Block,
           shape = Condition)) +
  geom_point() +
  scale_shape_manual(values = c(1, 3))

#' The function `exh_rerand_pval_blocking()` by default computes
#' _p_-values for the mean difference between two conditions
#' using exhaustive rerandomisations.
#' Its first parameter takes the outcome data,
#' its second the indices of the treatment group
#' (obtained below using `which()`),
#' its third a vector specifying the blocks.
exh_rerand_pval_blocking(d$Score, which(d$Condition == "intervention"), d$Block)
#' That is, the two-sided _p_-value for the mean difference is about 0.057.
#' In the histogram, the observed test statistic is highlighted by
#' the blue vertical line.
#' 
#' Instead, we could have run a test on the difference between the condition
#' medians like so, resulting in a two-sided _p_-value of about 0.19.
exh_rerand_pval_blocking(d$Score, which(d$Condition == "intervention"), d$Block,
                         statistic = median_diff)

#' # Monte Carlo rerandomisation
#' If there are many blocks, we need to use the Monte Carlo method instead.
#' This is implemented in the function `mc_pval_blocking()`:
mc_pval_blocking(d$Score, which(d$Condition == "intervention"), d$Block,
                 statistic = median_diff, M = 50000)
mc_pval_blocking(d$Score, which(d$Condition == "intervention"), d$Block,
                 statistic = mean_diff, M = 20000)

#' # Walkthrough
#' The `cannonball` package, available from https://github.com/janhove/cannonball/,
#' contains the function `walkthrough_blocking()` that walks the user through the
#' design and analysis of an experiment that uses blocking.
#' 
#' Consult the webpage https://github.com/janhove/cannonball for installation
#' instructions. (You'll also need the devtools package.)
#' 
#' Then load the `cannonball` package and run the following commands a couple
#' of times to observe how chance may affect the results of a study.
#' (The `pedant = TRUE` setting ensures that a Monte Carlo _p_-value is
#' used instead of an analytical shortcut (the _t_-test).)
#' ```{r, eval=FALSE}
#' walkthrough_blocking(n = 18, diff = 0.5, sd = 1, rho = 0.85, pedant = TRUE)
#' ```
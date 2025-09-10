#' ---
#' author: 'Jan Vanhove'
#' title: 'Examples of rerandomisation tests'
#' date: '2025/07/29'
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
#' The file `functions/rerandomisation_pvals.R` defines a few
#' R functions that can be used to compute rerandomisation _p_-values
#' in experiments where participants are randomly assigned to one of two
#' conditions.^[Programming a function that can perform exhaustive
#' rerandomisation when the experiment has more than two conditions is much
#' more difficult and wouldn't serve any pedagogical goal. For Monte Carlo
#' rerandomisation tests, such a function would _not_ be more difficult,
#' but I chose to keep the logic of the Monte Carlo function close
#' to the logic of the exhaustive rerandomisation function so that
#' you can better appreciate the similarities and differences between
#' them if you'd like to inspect the functions yourself.]
#' 
#' Load these functions like so:
library(here)
source(here("functions", "rerandomisation_pvals.R"))

#' # Exhaustive rerandomisation
#' ## Equal group sizes
#' Let's create a fictitious dataset of an experiment
#' in which 18 participants were randomly assigned to one of two groups
#' (9 participants per group).
#' These data are just randomly drawn numbers between 1 and 20 for both conditions.
set.seed(2025-07-02) # to make everything reproducible
d <- data.frame(
  outcome = sample(1:20, size = 18, replace = TRUE),
  group = rep(c("control", "treatment"), each = 9)
)
# A quick boxplot (without ggplot2)
boxplot(outcome ~ group, d)

#' The function `exh_rerand_pval()` by default computes
#' _p_-values for the mean difference between two conditions
#' using exhaustive rerandomisations.
#' Its first parameter takes the outcome data,
#' its second the indices of the treatment group
#' (obtained below using `which()`).
exh_rerand_pval(d$outcome, which(d$group == "treatment"))
#' That is, the two-sided _p_-value for the mean difference is about 0.84.
#' In the histogram, the observed test statistic is highlighted by
#' the blue vertical line.
#' 
#' Instead, we could have run a test on the difference between the condition
#' medians like so, resulting in a two-sided _p_-value of about 0.40.
exh_rerand_pval(d$outcome, which(d$group == "treatment"),
                statistic = median_diff)
#' Inspect the functions `mean_diff()` and `median_diff()`
#' in the R file if you want to define your own test statistics.
#' 
#' (If you already know a bit of R, you could try to adapt the `mean_diff()`
#' function so that it computes the difference between, for instance, trimmed means.)
#' 
#' ## Unequal group sizes
#' Nothing hinges on the group sizes being equal.
#' Here's an example with group sizes 7 and 11 instead of 9 and 9.
d <- data.frame(
  outcome = sample(1:20, size = 18, replace = TRUE),
  group = rep(c("control", "treatment"), times = c(7, 11))
)
boxplot(outcome ~ group, d)
exh_rerand_pval(d$outcome, which(d$group == "treatment"))


#' 
#' # Monte Carlo rerandomisation
#' For larger group sizes, we need to use the Monte Carlo method instead.
#' This is implemented in the function `mc_pval()`.
#' To illustrate its use, we'll use part of a dataset of a study I once ran.
#' It was hypothesised that the participant in the _ij-ei_ condition
#' would obtain higher scores than those in the _oe-u_ condition.
d <- read.csv(here("datasets", "vanhove2016_partial.csv"))
boxplot(PropCorrect ~ LearningCondition, d)
#' By default, 20,000 rerandomisations are generated (including the one 
#' we actually obtained), and the test concerns the mean difference.
mc_pval(d$PropCorrect, which(d$LearningCondition == "ij-ei"))
#' The right-sided _p_-value is of interest here and is about 0.001.
#' 
#' If we were interested in the difference between the medians, 
#' the right-sided _p_-value would be about 0.01.
#' Note that, as in the previous examples, the histogram for the differences
#' between the medians is much coarser than is the one for the differences
#' between the means. However, the _p_-values obtained are still valid.
mc_pval(d$PropCorrect, which(d$LearningCondition == "ij-ei"),
        statistic = median_diff, M = 50000) # using 50,000 rerandomisations

#' # Walkthrough
#' The `cannonball` package, available from https://github.com/janhove/cannonball/,
#' contains the function `walkthrough_p()` that walks the user through the
#' basic design and analysis of a simple experiment.
#' 
#' Consult the webpage https://github.com/janhove/cannonball for installation
#' instructions. (You'll also need the devtools package.)
#' 
#' Then load the `cannonball` package and run the following commands a couple
#' of times to observe how chance may affect the results of a study.
#' (The `pedant = TRUE` setting ensures that a Monte Carlo _p_-value is
#' used instead of an analytical shortcut (the _t_-test).)
#' ```{r, eval=FALSE}
#' walkthrough_p(n = 18, diff = 0.5, sd = 1, pedant = TRUE)
#' walkthrough_p(n = 22, diff = 0, sd = 1, pedant = TRUE)
#' ```
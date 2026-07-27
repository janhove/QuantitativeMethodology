#' ---
#' author: 'Jan Vanhove'
#' title: 'Examples of randomisation tests'
#' date: '2026/07/27'
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
#' The [`cannonball`](https://janhove.github.io/cannonball) contains
#' contains a function that allows you to compute _p_-values for experiments
#' in which participants are randomly assigned to one of two conditions
#' by means of a randomisation test.
#' 
#' Install the package as explained on the website, and then load it like so:
library(cannonball)
#' Also install and load the `here` package:
library(here)

#' # Exhaustive rerandomisation
#' ## Equal group sizes
#' Let's create a fictitious dataset of an experiment
#' in which 18 participants were randomly assigned to one of two groups
#' (9 participants per group).
#' These data are just randomly drawn numbers between 1 and 20 for both conditions.
set.seed(2026-07-27) # to make everything reproducible
d <- data.frame(
  outcome = sample(1:20, size = 18, replace = TRUE),
  group = rep(c("control", "treatment"), each = 9)
)
# A quick boxplot (without ggplot2)
boxplot(outcome ~ group, d)

#' By default, the function `rand_test()` computes _p_-values
#' between two conditions using exhausitive rerandomisation.
#' Its `outcome` parameter takes the outcome data;
#' the `treatment_idx` parameter takes the indices of the treatment group
#' (obtained below using `which()`),
#' and the `statistic` parameter specifies which test statistic should be used.
#' To compute _p_-values for the **mean difference**, we proceed as follows:
rand_test(d$outcome, which(d$group == "treatment"), statistic = mean_diff)
#' That is, the two-sided ('non-directional') _p_-value for 
#' the mean difference is about 0.96.
#' In the histogram, the observed test statistic is highlighted by
#' the blue vertical line.
#' 
#' Instead, we could have run a test on the difference between the condition
#' medians like so, resulting in a two-sided _p_-value of about 0.29.
rand_test(d$outcome, which(d$group == "treatment"), statistic = median_diff)
#' Some further test statistics are predefined (see `?test_statistics`),
#' e.g., the probability of superiority:
rand_test(d$outcome, which(d$group == "treatment"), statistic = prob_super)
#' If you already know a bit of R, you can try to adapt the `mean_diff()`
#' function so that it computes the difference between, for instance, trimmed means.
#' 
#' ## Unequal group sizes
#' Nothing hinges on the group sizes being equal.
#' Here's an example with group sizes 7 and 11 instead of 9 and 9.
d <- data.frame(
  outcome = sample(1:20, size = 18, replace = TRUE),
  group = rep(c("control", "treatment"), times = c(7, 11))
)
boxplot(outcome ~ group, d)
rand_test(d$outcome, which(d$group == "treatment"), statistic = mean_diff)
#' 
#' # Monte Carlo rerandomisation
#' For larger group sizes, we need to use the Monte Carlo method instead.
#' This, too, is implemented in the `rand_test()` function.
#' To illustrate its use, we'll use part of a dataset of a study I once ran.
#' It was hypothesised that the participant in the _ij-ei_ condition
#' would obtain higher scores than those in the _oe-u_ condition.
d <- read.csv(here("datasets", "vanhove2016_partial.csv"))
boxplot(PropCorrect ~ LearningCondition, d)
#' Use `exact = FALSE` to use the Monte Carlo method:
rand_test(d$PropCorrect, which(d$LearningCondition == "ij-ei"),
          statistic = mean_diff, exact = FALSE)
#' The right-sided _p_-value is of interest here and is about 0.0006.
#' 
#' If we were interested in the difference between the medians, 
#' the right-sided _p_-value would be about 0.01.
#' Note that, as in the previous examples, the histogram for the differences
#' between the medians is much coarser than is the one for the differences
#' between the means. However, the _p_-values obtained are still valid.
rand_test(d$PropCorrect, which(d$LearningCondition == "ij-ei"),
          statistic = median_diff, exact = FALSE, M = 50000) # 50,000 rerandomisations

rand_test(d$PropCorrect, which(d$LearningCondition == "ij-ei"),
          statistic = prob_super, exact = FALSE, M = 50000) # 50,000 rerandomisations
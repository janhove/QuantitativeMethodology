#' ---
#' author: 'Jan Vanhove'
#' title: 'Rerandomisation testing with blocking'
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
#' We've already used the `cannonball` and `here` packages in the previous
#' tutorial. We'll also need the `tidyverse` for a visualisation.
library(cannonball)
library(here)
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

#' We can use the `rand_test()` function and specify the `block` parameter
#' to run a randomisation test that takes the blocking structure into account.
rand_test(d$Score, which(d$Condition == "intervention"), d$Block, 
          statistic = mean_diff)
#' That is, the two-sided _p_-value for the mean difference is about 0.057.
#' In the histogram, the observed test statistic is highlighted by
#' the blue vertical line.
#' 
#' Instead, we could have run a test on the difference between the condition
#' medians like so, resulting in a two-sided _p_-value of about 0.19.
rand_test(d$Score, which(d$Condition == "intervention"), d$Block, 
          statistic = median_diff)

#' # Monte Carlo rerandomisation
#' If there are many blocks, we need to use the Monte Carlo method instead.
rand_test(d$Score, which(d$Condition == "intervention"), d$Block, 
          statistic = mean_diff, exact = FALSE)
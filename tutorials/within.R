#' ---
#' author: 'Jan Vanhove'
#' title: 'Analysis of within-subjects designs with two conditions'
#' date: '2025/08/19'
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
#' # AB/BA cross-over design
#' 
#' ## Preliminaries
#' The file `functions/ab_ba_crossover.R` contains an R function
#' that simulates data for a within-subjects design with two conditions
#' in which the participants are randomly assigned to one of two orders.
#' Such a design is also referred to as a AB/BA crossover design.
#' We'll also need the functions in `functions/rerandomisation_pvals.R`.
#' 
#' Load these functions like so:
library(here)
source(here("functions", "ab_ba_crossover.R"))
source(here("functions", "rerandomisation_pvals.R"))
#' We'll also use the `tidyverse` for some visualisations.
library(tidyverse)
#' Finally, set the random seed to make this tutorial reproducible.
set.seed(2025-08-19)
#'
#' ## Generate and inspect a dataset
#' We simulate a dataset with 18 participants, 9 of which are assigned to
#' the order AB and 9 to the order BA.
#' In terms of the Greek letters in Section 7.3 of the lecture notes,
#' $\beta$ is irrelevant, and we set $\tau = 0.23, \omega = 1, \kappa = 0$:
d <- generate_crossover(n = 18, effect_a = 0.23, effect_last = 1, carryover_a = 0) 
d
#' Note that the $A - B$ differences as well as the period differences
#' are precomputed; in real life, you'd have to compute these yourself.
#' 
#' Some visualisations. Note the stronger order effect. Nonetheless,
#' in the aggregate, across both orders, the A scores are better than the B scores.
#' The second plot is known as a Tukey mean--difference plot (also called
#' a Bland-Altman plot in medical circles).
d |> 
  mutate(ParticipantID = 1:n()) |> 
  pivot_longer(A:B, values_to = "Score", names_to = "Condition") |>
  ggplot(aes(x = Score,
             y = reorder(ParticipantID, Score),
             shape = Condition)) +
  geom_point() +
  scale_shape_manual(values = c(1, 3)) +
  xlab("Score") +
  ylab("Participant ID") +
  facet_grid(rows = vars(order), space = "free_y", scale = "free_y")

d |> 
  mutate(average = (A + B) / 2,
         difference = A - B) |> 
  ggplot(aes(x = average,
             y = difference)) +
  geom_point(shape = 1) +
  xlab("average score per participant: (A + B) / 2") +
  ylab("difference A-B") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(cols = vars(order))
#' And a quick numerical summary. The absolute value of the AB difference
#' is always the same as the absolute value of the period difference.
d_summary <- d |> 
  group_by(order) |> 
  summarise(
    mean_AB_difference = mean(difference_AB),
    mean_period_difference = mean(period_difference)
  )
d_summary
#' The treatment effect can be estimated as _half_ the difference
#' between the mean period differences:
(d_summary$mean_period_difference[1] - d_summary$mean_period_difference[2]) / 2
#' This is the same as the grand mean of the mean AB differences:
(d_summary$mean_AB_difference[1] + d_summary$mean_AB_difference[2]) / 2

#'
#' ## Exhaustive rerandomisation
#' We can reuse the functions in `functions/rerandomisation_pvals.R`.
#' If $A$ is considered the intervention and $B$ the control condition,
#' then this can be done like so:
exh_rerand_pval(d$period_difference / 2, which(d$order == "AB"))

#'
#' ## Monte Carlo rerandomisation
#' For larger numbers of pariticipants, the `mc_pval()` function can be used,
#' for instance,
d <- generate_crossover(n = 120, effect_a = 0.2, effect_last = 0.4, carryover_a = 0,
                        sd_error = 0.5) 

d |> 
  mutate(ParticipantID = 1:n()) |> 
  pivot_longer(A:B, values_to = "Score", names_to = "Condition") |>
  ggplot(aes(x = Score,
             y = reorder(ParticipantID, Score),
             shape = Condition)) +
  geom_point() +
  scale_shape_manual(values = c(1, 3)) +
  xlab("Score") +
  ylab("Participant ID") +
  facet_grid(rows = vars(order), space = "free_y", scale = "free_y")

d |> 
  mutate(average = (A + B) / 2,
         difference = A - B) |> 
  ggplot(aes(x = average,
             y = difference)) +
  geom_point(shape = 1) +
  xlab("average score per participant: (A + B) / 2") +
  ylab("difference A-B") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(cols = vars(order))

d_summary <- d |> 
  group_by(order) |> 
  summarise(
    mean_AB_difference = mean(difference_AB),
    mean_period_difference = mean(period_difference)
  )
d_summary
(d_summary$mean_period_difference[1] - d_summary$mean_period_difference[2]) / 2
mc_pval(d$period_difference / 2, which(d$order == "AB"))


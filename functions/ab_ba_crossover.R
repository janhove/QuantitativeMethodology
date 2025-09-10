#-------------------------------------------------------------------------------
# Function for simulating AB/BA crossover designs
# jan.vanhove@unifr.ch - 2025/07/31
#-------------------------------------------------------------------------------

generate_crossover <- function(
    n = 40, # total participants
    sd_baseline = 1, # st dev of baseline
    sd_error = 0.3, # st dev of measurement error or such like
    effect_a = 0.3, # benefit of A relative to B
    effect_last = 0.4, # benefit of last relative to first condition
    carryover_a = 0 # boost to B when occurring after A
) {
  baseline = rnorm(n, sd = sd_baseline)
  d <- data.frame(
    order = rep(c("AB", "BA"), times = c(ceiling(n/2), floor(n/2))),
    A = baseline + effect_a,
    B = baseline
  )
  d$B[d$order == "AB"] <- d$B[d$order == "AB"] + effect_last
  d$A[d$order == "BA"] <- d$A[d$order == "BA"] + effect_last
  d$B[d$order == "AB"] <- d$B[d$order == "AB"] + carryover_a
  d$A <- d$A + rnorm(n, sd = sd_error)
  d$B <- d$B + rnorm(n, sd = sd_error)
  d$difference_AB <- d$A - d$B
  d$period_difference <- d$difference_AB
  d$period_difference[d$order == "BA"] <- -d$difference_AB[d$order == "BA"]
  d
}
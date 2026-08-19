#-------------------------------------------------------------------------------
# Function for simulating data for a completely counterbalanced design with 3 conditions
# jan.vanhove@unifr.ch - 2026/08/19
#-------------------------------------------------------------------------------

generate_counterbalanced <- function(
    n = 40,                              # total participants
    treatment_effects = c(0, 0.5,-0.3),  # 3 condition baselines
    period_effects = c(0, 0.8, -1),      # 3 period baselines
    carryover_effects = rep(0, 3),  # the measurement immediately following A receives a boost of carryover_effects[1], ...,
                                    # the measurement immediately following C receives a boost of carryover_effects[3]
    sd_baseline = 1,         # st dev of subject baseline
    sd_error = 0.3,          # st dev of measurement error or such like
    square = NULL,           # a suitable Latin square; if none is provided, a random one will be created
    long_format = FALSE      # output data in long format?
) {
  ls1 <- rbind(
    c(1, 2, 3),
    c(2, 3, 1),
    c(3, 1, 2)
  )
  ls2 <- rbind(
    c(1, 3, 2),
    c(3, 2, 1),
    c(2, 1, 3)
  )
  n1 <- ceiling(n / 2)
  n2 <- floor(n / 2)
  d1 <- generate_latin_square(n1, k = 3, treatment_effects = treatment_effects,
                              period_effects = period_effects,
                              carryover_effects = carryover_effects,
                              sd_baseline = sd_baseline,
                              sd_error = sd_error,
                              square = ls1, long_format = long_format)
  d2 <- generate_latin_square(n2, k = 3, treatment_effects = treatment_effects,
                              period_effects = period_effects,
                              carryover_effects = carryover_effects,
                              sd_baseline = sd_baseline,
                              sd_error = sd_error,
                              square = ls2, long_format = long_format)
  d1$id <- as.numeric(d1$id)
  d2$id <- as.numeric(d2$id) + max(as.numeric(d1$id))
  d <- rbind(d1, d2)
  d$id <- as.factor(d$id)
  d
}
#-------------------------------------------------------------------------------
# Function for simulating data from AB/BA crossover designs
# jan.vanhove@unifr.ch - 2026/08/19
#-------------------------------------------------------------------------------

generate_crossover <- function(
    n = 40,             # total participants
    effect_a = 0.3,     # benefit of A relative to B
    effect_last = 0.4,  # benefit of last relative to first condition
    carryover_a = 0,    # boost to B when occurring after A
    sd_baseline = 1,    # st dev of baseline
    sd_error = 0.3,     # st dev of measurement error or such like
    long_format = FALSE # output data in long format?
) {
  # baseline = rnorm(n, sd = sd_baseline)
  # d <- data.frame(
  #   sequence = rep(c("AB", "BA"), times = c(ceiling(n/2), floor(n/2))),
  #   A = baseline + effect_a,
  #   B = baseline
  # )
  # d$B[d$sequence == "AB"] <- d$B[d$sequence == "AB"] + effect_last
  # d$A[d$sequence == "BA"] <- d$A[d$sequence == "BA"] + effect_last
  # d$B[d$sequence == "AB"] <- d$B[d$sequence == "AB"] + carryover_a
  # d$A <- d$A + rnorm(n, sd = sd_error)
  # d$B <- d$B + rnorm(n, sd = sd_error)
  # d$difference_AB <- d$A - d$B
  # d$period_difference <- d$difference_AB
  # d$period_difference[d$sequence == "BA"] <- -d$difference_AB[d$sequence == "BA"]
  # 
  # if (long_format) {
  #   d <- reshape(d, direction = "long", varying = c("A", "B"),
  #                v.names = "outcome", timevar = "condition",
  #                times = c("A", "B"),
  #                drop = c("difference_AB", "period_difference"))
  #   d$period <- ifelse(d$condition == "A" & d$sequence == "AB" |
  #                        d$condition == "B" & d$sequence == "BA",
  #                      1, 2)
  #   d <- d[order(d$sequence, d$id, d$condition), ]
  #   d <- d[, c("id", "sequence", "period", "condition", "outcome")]
  #   row.names(d) <- NULL
  #   d$id <- factor(d$id)
  #   d$period <- factor(d$period)
  # }
  # 
  # d
  
  generate_latin_square(n, k = 2, 
                        treatment_effects = c(effect_a, 0),
                        period_effects = c(0, effect_last),
                        carryover_effects = c(carryover_a, 0),
                        sd_baseline = sd_baseline,
                        sd_error = sd_error,
                        square = rbind(c(1, 2), c(2, 1)), 
                        long_format = long_format)
}

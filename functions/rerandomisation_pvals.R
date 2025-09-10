#-------------------------------------------------------------------------------
# Functions for computing rerandomisation p-values for two-group comparisons
# jan.vanhove@unifr.ch - 2025/07/30
#-------------------------------------------------------------------------------

# Useful test statistics -------------------------------------------------------

# Difference between group means
mean_diff <- function(outcome, treatment_idx) {
  mean(outcome[treatment_idx]) - mean(outcome[-treatment_idx])
}

# Difference between group medians
median_diff <- function(outcome, treatment_idx) {
  median(outcome[treatment_idx]) - median(outcome[-treatment_idx])
}

# Exhaustive rerandomisation in two groups -------------------------------------

# Only suitable for small samples (about 18 observations total).
exh_rerand_pval <- function(outcome, treatment_idx, statistic = mean_diff, plot = TRUE,
                            eps = .Machine$double.eps^(1/2)) {
  obs_stat <- statistic(outcome, treatment_idx)
  
  n <- length(outcome)
  rerandomisations <- combn(n, length(treatment_idx))
  H0_stats <- apply(rerandomisations, 2, statistic, outcome = outcome)
  
  if (plot) {
    hist(H0_stats, breaks = 30,
         xlab = "Test statistic",
         main = paste0("Test statistic in ", ncol(rerandomisations), " rerandomisations"))
    abline(v = obs_stat, col = "blue", lwd = 2)
  }
  
  # Account for machine precision in comparisons
  leftsided <- mean(H0_stats <= obs_stat + eps)
  rightsided <- mean(H0_stats >= obs_stat - eps)
  twosided <- min(2*min(leftsided, rightsided), 1)
  # Alternatively, use
  # twosided <- mean(abs(H0_stats) >= abs(obs_stat))
  # The version I used is also correct and has slightly more power.
  list("left-sided p-value" = leftsided,
       "right-sided p-value" = rightsided,
       "two-sided p-value" = twosided)
}

# Monte-Carlo p-values. For groups of arbitrary size (without blocking).
mc_pval <- function(outcome, treatment_idx, statistic = mean_diff, M = 20000, plot = TRUE,
                    eps = .Machine$double.eps^(1/2)) {
  obs_stat <- statistic(outcome, treatment_idx)
  H0_stats <- replicate(M - 1, {
    treatment_idx <- sample(1:length(outcome), length(treatment_idx))
    statistic(outcome, treatment_idx)
  })
  # include difference actually observed (makes math. guarantee exact)
  H0_stats <- c(obs_stat, H0_stats)
  if (plot) {
    hist(H0_stats, breaks = 30,
         xlab = "Test statistic",
         main = paste0("Test statistic in ", M, " rerandomisations"))
    abline(v = obs_stat, col = "blue", lwd = 2)
  }
  leftsided <- mean(H0_stats <= obs_stat + eps)
  rightsided <- mean(H0_stats >= obs_stat - eps)
  twosided <- min(2*min(leftsided, rightsided), 1)
  # Alternatively, use
  # twosided <- mean(abs(H0_stats) >= abs(obs_stat))
  # The version I used is also correct and has slightly more power.
  list("left-sided p-value" = leftsided,
       "right-sided p-value" = rightsided,
       "two-sided p-value" = twosided)
}

# Exhaustive rerandomisation when using blocking with 2 conditions and k blocks
exh_rerand_pval_blocking <- function(outcome, treatment_idx, block,
                                     statistic = mean_diff, plot = TRUE,
                                     eps = .Machine$double.eps^(1/2)) {
  obs_stat <- statistic(outcome, treatment_idx)
  k <- length(unique(block))
  block_indices <- lapply(1:k, function(b) which(block == b))
  all_combos <- expand.grid(lapply(block_indices, function(pair) pair))
  all_combos <- as.matrix(all_combos)
  H0_stats <- apply(all_combos, 1, statistic, outcome = outcome)
  
  if (plot) {
    hist(H0_stats, breaks = 30,
         xlab = "Test statistic",
         main = paste0("Test statistic in ", nrow(all_combos), " rerandomisations"))
    abline(v = obs_stat, col = "blue", lwd = 2)
  }
  
  # Account for machine precision in comparisons
  leftsided <- mean(H0_stats <= obs_stat + eps)
  rightsided <- mean(H0_stats >= obs_stat - eps)
  twosided <- min(2*min(leftsided, rightsided), 1)
  list("left-sided p-value" = leftsided,
       "right-sided p-value" = rightsided,
       "two-sided p-value" = twosided)
}

# Monte-Carlo p-values for blocked designs with 2 conditions and k blocks
# of two participants each.
mc_pval_blocking <- function(outcome, treatment_idx, block,
                             statistic = mean_diff, M = 20000, plot = TRUE,
                             eps = .Machine$double.eps^(1/2)) {
  obs_stat <- statistic(outcome, treatment_idx)
  k <- length(unique(block))
  rerandomise <- function(block) {
    # Pick one member per block as treated
    idx <- vector("numeric", length = k)
    for (b in 1:k) {
      idx[b] <- sample(which(block == b), 1)
    }
    idx
  }
  H0_stats <- replicate(M - 1, {
    treatment_idx <- rerandomise(block)
    statistic(outcome, treatment_idx)
  })
  H0_stats <- c(obs_stat, H0_stats)
  if (plot) {
    hist(H0_stats, breaks = 30,
         xlab = "Test statistic",
         main = paste0("Test statistic in ", M, " rerandomisations"))
    abline(v = obs_stat, col = "blue", lwd = 2)
  }
  leftsided <- mean(H0_stats <= obs_stat + eps)
  rightsided <- mean(H0_stats >= obs_stat - eps)
  twosided <- min(2*min(leftsided, rightsided), 1)
  list("left-sided p-value" = leftsided,
       "right-sided p-value" = rightsided,
       "two-sided p-value" = twosided)
}

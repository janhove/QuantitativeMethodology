set.seed(2025-07-30)
n <- 32
k <- n/2
pretest <- sort(rnorm(n))
control <- pretest + rnorm(n, sd = 0.4)
intervention <- control + 0.3
block <- rep(1:(n/2), each = 2)
random_indices <- sapply(1:k, function(b) {
  which(block == b) |> sample(1)
})

d <- tibble(
  Nr = 1:n,
  Block = block
)
d$Condition <- "control"
d$Condition[d$Nr %in% random_indices] <- "intervention"
d$Score <- control
d$Score[d$Condition == "intervention"] <- intervention[d$Condition == "intervention"]
d <- d[, -1]

write_csv(d, here("datasets", "example_blocking.csv"))

exh_rerand_pval_blocking(d$Score, which(d$Condition == "intervention"), d$Block)
mc_pval_blocking(d$Score, which(d$Condition == "intervention"), d$Block)

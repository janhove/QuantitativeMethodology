#-------------------------------------------------------------------------------
# Function for simulating data for a Latin squares design
# jan.vanhove@unifr.ch - 2026/08/19
#-------------------------------------------------------------------------------

random_latin_square <- function(k) {
  base <- outer(0:(k - 1), 0:(k - 1), function(i, j) (i + j) %% k) + 1
  base <- base[sample(k), , drop = FALSE]
  base <- base[, sample(k), drop = FALSE]
  symbol_perm <- sample(k)               
  matrix(symbol_perm[base], nrow = k, ncol = k)
}

is_latin_square <- function(square) {
  if (ncol(square) != nrow(square)) {
    return(FALSE)
  }
  if (ncol(square) <= 0) {
    return(FALSE)
  }
  first_row <- square[1, ] |> unique() |> sort()
  if (length(first_row) != nrow(square)) {
    return(FALSE)
  }
  for (i in seq_len(ncol(square))) {
    if (!isTRUE(all.equal(square[i, ] |> unique() |> sort(), first_row))) return(FALSE)
  }
  TRUE
}

generate_latin_square <- function(
    n = 40,                                    # total participants
    k = 4,                                     # number of conditions
    treatment_effects = c(0, 0.5, 0.2, -0.3),  # k condition baselines
    period_effects = c(0, 0.3, 0.8, -1),       # k period baselines
    carryover_effects = rep(0, 4),  # the measurement immediately following A receives a boost of carryover_effects[1], ...,
                                    # the measurement immediately following D receives a boost of carryover_effects[4]
    sd_baseline = 1,         # st dev of subject baseline
    sd_error = 0.3,          # st dev of measurement error or such like
    square = NULL,           # a suitable Latin square; if none is provided, a random one will be created
    long_format = FALSE      # output data in long format?
) {
  treatments <- LETTERS[1:k]
  
  if (is.null(treatment_effects)) {
    treatment_effects <- setNames(numeric(k), treatments)
  }
  if (is.null(period_effects)) {
    period_effects <- numeric(k)
  }
  if (is.null(carryover_effects)) {
    carryover_effects <- setNames(numeric(k), treatments)
  }
  stopifnot(length(treatment_effects) == k, length(period_effects) == k,
            length(carryover_effects) == k)
  if (is.null(names(treatment_effects))) names(treatment_effects) <- treatments
  if (is.null(names(carryover_effects))) names(carryover_effects) <- treatments
  
  if (is.null(square)) {
    square <- random_latin_square(k)
  }
  stopifnot(nrow(square) == k, ncol(square) == k)
  seq_labels <- apply(square, 1, function(row) paste(treatments[row], collapse = ""))
  
  # Assign subjects to sequences
  seq_id <- rep(1:k, length.out = n) |> sample()
  baseline <- rnorm(n, sd = sd_baseline)
  
  # Build one row per subject x period
  out <- vector("list", n)
  for (i in seq_len(n)) {
    row <- square[seq_id[i], ]
    cond <- treatments[row] # treatment per period, in period order
    period <- seq_len(k)
    prev_cond <- c(NA, cond[-k]) # preceding period's treatment
    carry <- ifelse(is.na(prev_cond), 0, carryover_effects[prev_cond])
    
    outcome <- baseline[i] +
      treatment_effects[cond] +
      period_effects[period] +
      carry +
      rnorm(k, sd = sd_error)
    
    out[[i]] <- data.frame(
      id = i,
      sequence = seq_labels[seq_id[i]],
      period = period,
      condition = cond,
      outcome = as.numeric(outcome)
    )
  }
  d <- do.call(rbind, out)
  d$id <- factor(d$id)
  d$period <- factor(d$period)
  
  if (long_format) return(d)
  
  wide <- reshape(d[, c("id", "sequence", "condition", "outcome")],
                  direction = "wide", idvar = c("id", "sequence"),
                  timevar = "condition", v.names = "outcome")
  names(wide) <- sub("^outcome\\.", "", names(wide))
  wide <- wide[, c("id", "sequence", treatments)]
  row.names(wide) <- NULL
  wide
}
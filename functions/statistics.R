ranksum <- function(x, group) {
  if (sort(unique(group)) != c(0, 1)) {
    stop("group should only contain 0s and 1s.")
  }
  ranks <- rank(x)
  sum(ranks[group == 1])
}
probsup <- function(x, group) {
  if (sort(unique(group)) != c(0, 1)) {
    stop("group should only contain 0s and 1s.")
  }
  n0 <- sum(group == 0)
  n1 <- sum(group == 1)
  (ranksum(x, group) - n1*(n1 + 1)/2) / (n0 * n1)
}
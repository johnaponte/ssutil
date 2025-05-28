# Test the A49 and A50 formula

pcs_A49 <- function(p1, d, n, k) {
  # p1: success prob. in the best group
  # d: difference between best and others
  # n: sample size per group
  # k: number of groups
  
  p2 <- p1 - d
  # precompute b1[j], b2[j], B[j] = P(X2 <= j-1)
  b1 <- dbinom(0:n, size = n, prob = p1)
  b2 <- dbinom(0:n, size = n, prob = p2)
  B  <- pbinom((0:n) - 1, size = n, prob = p2)
  B[1] <- 0  # define B[-1] = 0
  
  # doble suma: j=0..n, i=0..k-1
  res <- 0
  for (j in 0:n) {
    inner <- 0
    for (i in 0:(k-1)) {
      inner <- inner +
        choose(k-1, i) / (1 + i) *
        b2[j+1]^i *
        B[j+1]^(k-1-i)
    }
    res <- res + b1[j+1] * inner
  }
  res
}

pcs_A49b <- function(p1, dif, npergroup, ngroups) {
  # probability in other groups
  p2 <- p1 - dif
  
  # b1[j] = P(X_(1) = j) = dbinom(j; npergroup, p1)
  b1 <- dbinom(0:npergroup, size = npergroup, prob = p1)
  # b2[j] = P(X_(2) = j) = dbinom(j; npergroup, p2)
  b2 <- dbinom(0:npergroup, size = npergroup, prob = p2)
  # B[j]  = P(X_(2) <= j-1) = pbinom(j-1; npergroup, p2)
  B  <- pbinom((0:npergroup) - 1, size = npergroup, prob = p2)
  B[1] <- 0  # define B_{2, -1} = 0
  
  total <- 0
  for (j in 0:npergroup) {
    inner_sum <- 0
    for (i in 0:(ngroups - 1)) {
      inner_sum <- inner_sum +
        choose(ngroups - 1, i) / (1 + i) *
        (b2[j + 1]^i) *
        (B[j + 1]^(ngroups - 1 - i))
    }
    total <- total + b1[j + 1] * inner_sum
  }
  total
}



# pcs_A50: caso k = 4 expandido (A50)
pcs_A50 <- function(p1, d, n) {
  p2 <- p1 - d
  b1 <- dbinom(0:n, size = n, prob = p1)
  b2 <- dbinom(0:n, size = n, prob = p2)
  B  <- pbinom((0:n) - 1, size = n, prob = p2)
  B[1] <- 0
  
  term1 <- sum(b1 * B^3)
  term2 <- (3/2) * sum(b1 * b2 * B^2)
  term3 <- sum(b1 * b2^2 * B)
  term4 <- (1/4) * sum(b1 * b2^3)
  
  term1 + term2 + term3 + term4
}

# --- Test rápido ---
p1 <- 0.8; d <- 0.1; n <- 30
res49 <- pcs_A49(p1, d, n, k = 4)
res50 <- pcs_A50(p1, d, n)
res49b <- pcs_A49b(p1,d,n,4)
cat("A49 (k=4):", res49, "\nA50:", res50, "\nA49b: ", res49b)
# deben coincidir (hasta errores numéricos muy pequeños)



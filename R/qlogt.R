# Quantile Function for the Log-Student's t Distribution
qlogt <- function(p, L, mu, sigma, df) {
  L + exp(qt(p, df = df) * sigma + mu)
}

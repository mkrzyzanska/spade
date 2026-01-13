hdrstd <- function(qfun, credMass, ...) {
  f <- function(p) qfun(p + credMass, ...) - qfun(p,...)
  opt <- optimize(f, interval = c(0, 1 - credMass))
  p <- opt$minimum
  c(lower = qfun(p, ...), upper = qfun(p + credMass, ...))
}


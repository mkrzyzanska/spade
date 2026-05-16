# Plot the elicited cumulative probabilities

Plots the elicited cumulative probabilities and, optionally, a fitted
CDF. Elicited are shown as filled circles, and limits are shown as clear
circles.

## Usage

``` r
makeCDFPlot(
  lower,
  v,
  p,
  upper,
  fontsize = 12,
  fit = NULL,
  dist = NULL,
  showFittedCDF = FALSE,
  showQuantiles = FALSE,
  ql = 0.05,
  qu = 0.95,
  ex = 1,
  sf = 3,
  xaxisLower = lower,
  xaxisUpper = upper,
  xlab = "x",
  ylab = expression(P(X <= x)),
  min_val = NULL,
  max_val = NULL
)
```

## Arguments

- lower:

  lower limit for the uncertain quantity

- v:

  vector of values, for each value x in Pr(X\<=x) = p in the set of
  elicited probabilities

- p:

  vector of probabilities, for each value p in Pr(X\<=x) = p in the set
  of elicited probabilities

- upper:

  upper limit for the uncertain quantity

- fontsize:

  font size to be used in the plot

- fit:

  object of class `elicitation`

- dist:

  the fitted distribution to be plotted. Options are `"normal"`, `"t"`,
  `"skewnormal"`, `"gamma"`, `"lognormal"`, `"logt"`,`"beta"`,
  `"mirrorgamma"`, `"mirrorlognormal"`, `"mirrorlogt"` `"hist"` (for a
  histogram fit)

- showFittedCDF:

  logical. Should a fitted distribution function be displayed?

- showQuantiles:

  logical. Should quantiles from the fitted distribution function be
  displayed?

- ql:

  a lower quantile to be displayed.

- qu:

  an upper quantile to be displayed.

- ex:

  if the object `fit` contains judgements from multiple experts, which
  (single) expert's judgements to show.

- sf:

  number of significant figures to be displayed.

- xaxisLower:

  lower limit for the x-axis.

- xaxisUpper:

  upper limit for the x-axis.

- xlab:

  x-axis label.

- ylab:

  y-axis label.

## Examples

``` r

if (FALSE) { # \dontrun{
vQuartiles <- c(30, 35, 45)
pQuartiles<- c(0.25, 0.5, 0.75)
myfit <- fitdist(vals = vQuartiles, probs = pQuartiles, lower = 0)
makeCDFPlot(lower = 0, v = vQuartiles, p = pQuartiles,
 upper = 100, fit = myfit, dist = "gamma",
 showFittedCDF = TRUE, showQuantiles = TRUE)


} # }
```

# Plot the fitted density function for one or more experts

Plots the fitted density function for one or more experts. Can also plot
a fitted linear pool if more than one expert. If plotting the density
function of one expert, or the linear pool only, can also indicated
desired lower and upper fitted quantiles.

## Usage

``` r
plotfit(
  fit,
  d = "best",
  xl = -Inf,
  xu = Inf,
  yl = 0,
  yu = NA,
  ql = NA,
  qu = NA,
  lp = FALSE,
  ex = NA,
  sf = 3,
  ind = TRUE,
  lpw = 1,
  fs = 12,
  lwd = 1,
  xlab = "x",
  ylab = expression(f[X](x)),
  legend_full = TRUE,
  percentages = FALSE,
  returnPlot = FALSE,
  showPlot = TRUE,
  startDate = NA,
  endDate = NA,
  nBins = NA,
  ybreaks = NA,
  chips = NA
)
```

## Arguments

- fit:

  An object of class `elicitation`.

- d:

  The distribution fitted to each expert's probabilities. Options are
  `"normal"`, `"t"`, `"skewnormal"`, `"gamma"`, `"lognormal"`,
  `"logt"`,`"beta"`, `"mirrorgamma"`, `"mirrorlognormal"`,
  `"mirrorlogt"` `"hist"` (for a histogram fit), and `"best"` (for best
  fitting)

- xl:

  The lower limit for the x-axis. The default is the 0.001 quantile of
  the fitted distribution (or the 0.001 quantile of a fitted normal
  distribution, if a histogram fit is chosen).

- xu:

  The upper limit for the x-axis. The default is the 0.999 quantile of
  the fitted distribution (or the 0.999 quantile of a fitted normal
  distribution, if a histogram fit is chosen).

- yl:

  The lower limit for the y-axis. Default value is 0.

- yu:

  The upper limit for the y-axis. Will be set automatically if not
  specified.

- ql:

  A lower quantile to be indicated on the density function plot. Only
  displayed when plotting the density function for a single expert.

- qu:

  An upper quantile to be indicated on the density function plot. Only
  displayed when plotting the density function for a single expert.

- lp:

  For multiple experts, set `lp = TRUE` to plot a linear pool.

- ex:

  If judgements have been elicited from multiple experts, but a density
  plot for one expert only is required, the expert to be used in the
  plot.

- sf:

  The number of significant figures to be displayed for the parameter
  values.

- ind:

  If plotting a linear pool, set `ind = FALSE` to suppress plotting of
  the individual density functions.

- lpw:

  A vector of weights to be used in linear pool, if unequal weighting is
  desired.

- fs:

  The font size used in the plot.

- lwd:

  The line width used in the plot.

- xlab:

  A string or expression giving the x-axis label.

- ylab:

  A string or expression giving the y-axis label.

- legend_full:

  If plotting a linear pool, set `ind = TRUE` for each expert to be
  plotted with a different colour, and `ind = FALSE` for each expert to
  be plotted with the same colour, reducing the legend size.

- percentages:

  Set to `TRUE` to use percentages on the x-axis.

- returnPlot:

  Set to `TRUE` to return the plot as a ggplot object.

- showPlot:

  Set to `FALSE` to suppress displaying the plot.

## Author

Jeremy Oakley <j.oakley@sheffield.ac.uk>

## Examples

``` r
if (FALSE) { # \dontrun{
# Two experts
# Expert 1 states P(X<30)=0.25, P(X<40)=0.5, P(X<50)=0.75
# Expert 2 states P(X<20)=0.25, P(X<25)=0.5, P(X<35)=0.75
# Both experts state 0<X<100.

v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
p <- c(0.25, 0.5, 0.75)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)

# Plot both fitted densities, using the best fitted distribution
plotfit(myfit)

# Plot a fitted beta distribution for expert 2, and show 5th and 95th percentiles
plotfit(myfit, d = "beta", ql = 0.05, qu = 0.95, ex = 2)


# Plot a linear pool, giving double weight to expert 1
plotfit(myfit,  lp = T, lpw = c(2,1))


# Plot a linear pool, giving double weight to expert 1,
# show 5th and 95th percentiles, surpress plotting of individual distributions,
# and force use of Beta distributions
plotfit(myfit, d = "beta",  lp = T, lpw = c(2,1), ql = 0.05, qu = 0.95, ind=FALSE )
} # }
```

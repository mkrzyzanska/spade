# Fit distributions to elicited probabilities

Takes elicited probabilities as inputs, and fits parametric
distributions using least squares on the cumulative distribution
function. If separate judgements from multiple experts are specified,
the function will fit one set of distributions per expert.

## Usage

``` r
fitdist(
  vals,
  probs,
  lower = -Inf,
  upper = Inf,
  weights = 1,
  tdf = 3,
  expertnames = NULL,
  excludelogt = FALSE
)
```

## Arguments

- vals:

  A vector of elicited values for one expert, or a matrix of elicited
  values for multiple experts (one column per expert). Note that the an
  elicited judgement about X should be of the form P(X\<= valsi,j) =
  probsi,j

- probs:

  A vector of elicited probabilies for one expert, or a matrix of
  elicited values for multiple experts (one column per expert). A single
  vector can be used if the probabilities are the same for each expert.
  For each expert, there should be at least one non-zero probability
  less than 0.4, and at least one elicited probability less and 1 and
  greater than 0.6. Exponential distributions can be fitted by
  specifying one limit (`lower` or `upper`) and one probability between
  0 and 1.

- lower:

  A single lower limit for the uncertain quantity X, or a vector of
  different lower limits for each expert. Specifying a lower limit will
  allow the fitting of distributions bounded below.

- upper:

  A single upper limit for the uncertain quantity X, or a vector of
  different lower limits for each expert. Specifying both a lower limit
  and an upper limit will allow the fitting of a Beta distribution.

- weights:

  A vector or matrix of weights corresponding to vals if weighted least
  squares is to be used in the parameter fitting.

- tdf:

  The number of degrees of freedom to be used when fitting a
  t-distribution.

- expertnames:

  Vector of names to use for each expert.

- excludelogt:

  Set to TRUE to exclude log-t and mirror log-t when identifying best
  fitting distribution.

## Value

An object of class `elicitation`. This is a list containing the elements

- Normal:

  Parameters of the fitted normal distributions.

- Student.t:

  Parameters of the fitted t distributions. Note that (X - location) /
  scale has a standard t distribution. The degrees of freedom is not
  fitted; it is specified as an argument to `fitdist`.

- Skewnormal:

  Parameters of the fitted skew-normal distribution. The skew-normal
  distribution is implemented using the sn package. See sn::dsn for
  details. This distribution requires at least three elicited
  probabilities, including at least one in each interval (0, 0.4) and
  (0.6, 1).

- Gamma:

  Parameters of the fitted gamma distributions. Note that E(X - `lower`)
  = shape / rate.

- Log.normal:

  Parameters of the fitted log normal distributions: the mean and
  standard deviation of log (X - `lower`).

- Log.Student.t:

  Parameters of the fitted log student t distributions. Note that
  (log(X- `lower`) - location) / scale has a standard t distribution.
  The degrees of freedom is not fitted; it is specified as an argument
  to `fitdist`.

- Beta:

  Parameters of the fitted beta distributions. X is scaled to the
  interval 0,1 via Y = (X - `lower`)/(`upper` - `lower`), and E(Y) =
  shape1 / (shape1 + shape2).

- mirrorgamma:

  Parameters of ('mirror') gamma distributions fitted to Y =
  `upper` - X. Note that E(Y) = shape / rate.

- mirrorlognormal:

  Parameters of ('mirror') log normal distributions fitted to Y =
  `upper` - X.

- mirrorlogt:

  Parameters of ('mirror') log Student-t distributions fitted to Y =
  `upper` - X. Note that (log(Y) - location) / scale has a standard t
  distribution. The degrees of freedom is not fitted; it is specified as
  an argument to `fitdist`.

- ssq:

  Sum of squared errors for each fitted distribution and expert. Each
  error is the difference between an elicited cumulative probability and
  the corresponding fitted cumulative probability.

- best.fitting:

  The best fitting distribution for each expert, determined by the
  smallest sum of squared errors. Note that with three judgements only,
  this is likely to be the skew-normal, as this is a three parameter
  distribution.

- vals:

  The elicited values used to fit the distributions.

- probs:

  The elicited probabilities used to fit the distributions.

- limits:

  The lower and upper limits specified by each expert (+/- Inf if not
  specified).

## Note

The least squares parameter values are found numerically using the
`optim` command. Starting values for the distribution parameters are
chosen based on a simple normal approximation: linear interpolation is
used to estimate the 0.4, 0.5 and 0.6 quantiles, and starting parameter
values are chosen by setting E(X) equal to the 0.5th quantile, and
Var(X) = (0.6 quantile - 0.4 quantile)^2 / 0.25. Note that the arguments
`lower` and `upper` are not included as elicited values on the
cumulative distribution function. To include a judgement such as
P(X\<=a)=0, the values a and 0 must be included in `vals` and `probs`
respectively.

## Author

Jeremy Oakley <j.oakley@sheffield.ac.uk>

## Examples

``` r
if (FALSE) { # \dontrun{
# One expert, with elicited probabilities
# P(X<20)=0.25, P(X<30)=0.5, P(X<50)=0.75
# and X>0.
v <- c(20,30,50)
p <- c(0.25,0.5,0.75)
fitdist(vals=v, probs=p, lower=0)

# Now add a second expert, with elicited probabilities
# P(X<55)=0.25, P(X<60=0.5), P(X<70)=0.75
v <- matrix(c(20,30,50,55,60,70),3,2)
p <- c(0.25,0.5,0.75)
fitdist(vals=v, probs=p, lower=0)

# Two experts, different elicited quantiles and limits.
# Expert A: P(X<50)=0.25, P(X<60=0.5), P(X<65)=0.75, and provides bounds 10<X<100
# Expert B: P(X<40)=0.33, P(X<50=0.5), P(X<60)=0.66, and provides bounds 0<X
v <- matrix(c(50,60,65,40,50,60),3,2)
p <- matrix(c(.25,.5,.75,.33,.5,.66),3,2)
l <- c(10,0)
u <- c(100, Inf)
fitdist(vals=v, probs=p, lower=l, upper=u)
} # }
```

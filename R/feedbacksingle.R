feedbacksingle <-
function(fit, quantiles =  NA, values = NA, sf = 3, ex = 1,hdr.prob=90){
  credMass=hdr.prob/100
	n.distributions <- 13
	distribution.names <- c("normal", "t", "skewnormal", "gamma", "lognormal",
	                        "logt", "beta", "hist",
	                        "mirrorgamma", "mirrorlognormal",
	                        "mirrorlogt","NS","MP")

	# Fitted quantiles ----

	if(is.na(quantiles[1]) == F ){
		report.elicited.q <- F
	}else{
		quantiles <- fit$probs[ex,]
		report.elicited.q <- T
	}


	Mq <- matrix(NA, length(quantiles), n.distributions)

	colnames(Mq) <- distribution.names
	if(!is.na(fit$ssq[ex, "normal"])){
	  Mq[, "normal"] <- qnorm(quantiles, fit$Normal[ex,1], fit$Normal[ex,2])
	}
	if(!is.na(fit$ssq[ex, "t"])){
	  Mq[, "t"] <- qt(quantiles, fit$Student.t[ex,3]) * fit$Student.t[ex,2] +
	  fit$Student.t[ex,1]
	}
	if(!is.na(fit$ssq[ex, "skewnormal"])){
	Mq[, "skewnormal"] <- sn::qsn(quantiles, xi = fit$Skewnormal[ex,1],
	                              omega = fit$Skewnormal[ex,2],
	                              alpha = fit$Skewnormal[ex,3])
	}


	  if(!is.na(fit$ssq[ex, "gamma"])){
	  Mq[, "gamma"] <- fit$limits[ex,1] +
		  qgamma(quantiles, fit$Gamma[ex,1], fit$Gamma[ex,2])
	  }
	  if(!is.na(fit$ssq[ex, "lognormal"])){
		Mq[, "lognormal"] <- fit$limits[ex,1] +
		  qlnorm(quantiles, fit$Log.normal[ex,1], fit$Log.normal[ex,2])
	  }
	  if(!is.na(fit$ssq[ex, "logt"])){
		Mq[, "logt"] <- fit$limits[ex,1] +
		  exp( qt(quantiles, fit$Log.Student.t[ex,3]) * fit$Log.Student.t[ex,2] +
		         fit$Log.Student.t[ex, 1])}

	if(!is.na(fit$ssq[ex, "beta"])){
			Mq[, "beta"] <- fit$limits[ex,1] +
			  (fit$limits[ex,2] - fit$limits[ex,1]) *
			  qbeta(quantiles, fit$Beta[ex,1], fit$Beta[ex,2] )
	}
			if(fit$limits[ex,1] > - Inf & fit$limits[ex,2] < Inf) {
			Mq[, "hist"] <- qhist(quantiles,
			                 c(fit$limits[ex, "lower"], fit$vals[ex, ],
			                   fit$limits[ex, "upper"]),
			                 c(0, fit$probs[ex, ], 1 )
			                 )
		}

	if(!is.na(fit$ssq[ex, "mirrorgamma"])){
	  Mq[, "mirrorgamma"] <- fit$limits[ex,2] -
	    qgamma(1 - quantiles, fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])
	}
	if(!is.na(fit$ssq[ex, "mirrorlognormal"])){
	  Mq[, "mirrorlognormal"] <- fit$limits[ex,2] -
	    qlnorm(1 - quantiles, fit$mirrorlognormal[ex,1],
	           fit$mirrorlognormal[ex,2])
	}
	if(!is.na(fit$ssq[ex, "mirrorlogt"])){
	  Mq[, "mirrorlogt"] <- fit$limits[ex,2] -
	    exp( qt(1 - quantiles, fit$mirrorlogt[ex,3]) * fit$mirrorlogt[ex,2] +
	           fit$mirrorlogt[ex, 1])
	}

     # B-spline Quantiles
  if(!all(is.na(fit$Bspline[ex,]))){ # Check if B-spline was fitted
    bspline_params <- fit$Bspline[ex,]
    x_vals <- seq(min(fit$vals[ex,], na.rm = TRUE), max(fit$vals[ex,], na.rm = TRUE), length.out = 100) # Or more points
    n_knots <- sum(!is.na(bspline_params))
    b_spline_basis <- splines::bs(x_vals, knots = quantile(fit$vals[ex,], probs = seq(0, 1, length.out = n_knots+1)[2:n_knots]), degree = 3, intercept = TRUE)
    y_vals <- b_spline_basis %*% bspline_params[!is.na(bspline_params)]

    Mq[, "bspline"] <- approx(y_vals, x_vals, xout = quantiles)$y # Interpolation for quantiles

  }

    if(!(is.null(fit$NS))){

        a <-approx(fit$NS$cdf,fit$NS$values,xout=quantiles)$y
        Mq[,"NS"] <- approx(fit$NS$cdf,fit$NS$values,xout=quantiles)$y

        }

     if(!(is.null(fit$MP))){

        a <-approx(fit$MP$cdf,fit$MP$values,xout=quantiles)$y
        Mq[,"MP"] <- approx(fit$MP$cdf,fit$MP$values,xout=quantiles)$y

        }


	# Fitted probabilities ----
	if(is.na(values[1]) == F ){
		valuesMatrix <- matrix(values,
		                       nrow = length(values),
		                       ncol = n.distributions)
		report.elicited.p <- F
		}else{
		  valuesMatrix <- matrix(fit$vals[ex,],
		                         nrow = length(fit$vals[ex,]),
		                         ncol = n.distributions)
		  values <- fit$vals[ex, ]
		report.elicited.p <- T
		}

	colnames(valuesMatrix) <- distribution.names

	Mp <- matrix(NA, nrow(valuesMatrix), ncol(valuesMatrix))
	colnames(Mp) <- distribution.names

	if(!is.na(fit$ssq[ex, "t"])){
	valuesMatrix[, "t"] <- (valuesMatrix[, "t"] - fit$Student.t[ex,1]) /
	  fit$Student.t[ex,2]

	Mp[,"t"] <- pt(valuesMatrix[, "t"], fit$Student.t[ex,3])
	}

	if(!is.na(fit$ssq[ex, "normal"])){
	  Mp[, "normal"] <- pnorm(valuesMatrix[, "normal"],
	                          fit$Normal[ex,1],
	                          fit$Normal[ex,2])

	}

	if(!is.na(fit$ssq[ex, "skewnormal"])){
	  Mp[, "skewnormal"] <- sn::psn(valuesMatrix[, "skewnormal"],
	                                xi = fit$Skewnormal[ex,1],
	                                omega = fit$Skewnormal[ex,2],
	                                alpha = fit$Skewnormal[ex,3])
	}

	if(!is.na(fit$ssq[ex, "gamma"])){
	  valuesMatrix[, "gamma"] <-
	    valuesMatrix[, "gamma"] - fit$limits[ex,1]
	  Mp[, "gamma"] <- pgamma(valuesMatrix[, "gamma"],
	                          fit$Gamma[ex,1], fit$Gamma[ex,2])
	}

	if(!is.na(fit$ssq[ex, "lognormal"])){
	  valuesMatrix[, "lognormal"] <-
	    valuesMatrix[, "lognormal"] - fit$limits[ex,1]
	  Mp[, "lognormal"] <- plnorm(valuesMatrix[, "lognormal"],
	                              fit$Log.normal[ex,1], fit$Log.normal[ex,2])
	}

	if(!is.na(fit$ssq[ex, "logt"])){
	  valuesMatrix[, "logt"] <- (log(abs(valuesMatrix[, "logt"] - fit$limits[ex,1])) -
	                               fit$Log.Student.t[ex,1]) / fit$Log.Student.t[ex,2]
	  # avoid log of negative values. Set probability to 0 if X below lower limit
	  Mp[, "logt"] <- pt(valuesMatrix[, "logt"], fit$Log.Student.t[ex,3])
	  Mp[values <= fit$limits[ex, 1],  "logt"] <- 0
	}

	if(!is.na(fit$ssq[ex, "beta"])){
	  valuesMatrix[, "beta"] <- (valuesMatrix[, "beta"] - fit$limits[ex,1]) /
	    (fit$limits[ex,2] - fit$limits[ex,1])
	  Mp[, "beta"] <- pbeta(valuesMatrix[, "beta"], fit$Beta[ex,1], fit$Beta[ex,2])
	}

	if(!is.na(fit$ssq[ex, "mirrorgamma"])){
	  valuesMatrix[, "mirrorgamma"] <- fit$limits[ex,2] -
	    valuesMatrix[, "mirrorgamma"]
	  Mp[, "mirrorgamma"] <- 1 - pgamma(valuesMatrix[, "mirrorgamma"],
	                                    fit$mirrorgamma[ex,1], fit$mirrorgamma[ex,2])
	}

	if(!is.na(fit$ssq[ex, "mirrorlognormal"])){
	  valuesMatrix[, "mirrorlognormal"] <- fit$limits[ex,2] -
	    valuesMatrix[, "mirrorlognormal"]
	  Mp[, "mirrorlognormal"] <- 1 - plnorm(valuesMatrix[, "mirrorlognormal"],
	                                        fit$mirrorlognormal[ex,1], fit$mirrorlognormal[ex,2])
	}

	if(!is.na(fit$ssq[ex, "mirrorlogt"])){
	  valuesMatrix[, "mirrorlogt"] <- (log(abs(fit$limits[ex,2] - valuesMatrix[, "mirrorlogt"])) -
	                                     fit$mirrorlogt[ex,1]) / fit$mirrorlogt[ex,2]
	  Mp[, "mirrorlogt"] <- 1 - pt(valuesMatrix[, "mirrorlogt"],
	                               fit$mirrorlogt[ex,3])

	  # set to 0 for log-T, if x is above upper limit
	  Mp[values >= fit$limits[ex, 2],  "mirrorlogt"] <- 0
	}

	if(fit$limits[ex,1] > - Inf & fit$limits[ex,2] <  Inf){

	    Mp[, "hist"] <- phist(valuesMatrix[, "hist"],
	                          c(fit$limits[ex, "lower"], fit$vals[ex, ], fit$limits[ex, "upper"]),
	                          c(0, fit$probs[ex, ], 1 ))
	  }


     if(!(is.null(fit$NS))){

        Mp[,"NS"] <- predict(fit$NS$fit.cdf,newdata=data.frame(extended_vals=values))

        }

    if(!(is.null(fit$MP))){

        Mp[,"MP"] <- predict(fit$MP$fit.cdf,newdata=data.frame(extended_vals=values))

        }


 # B-spline Probabilities
    if(!all(is.na(fit$Bspline[ex,]))){ # Check if B-spline was fitted
    bspline_params <- fit$Bspline[ex,]
    x_vals <- seq(min(fit$vals[ex,], na.rm = TRUE), max(fit$vals[ex,], na.rm = TRUE), length.out = 100) # Or more points
    n_knots <- sum(!is.na(bspline_params))
    b_spline_basis <- splines::bs(x_vals, knots = quantile(fit$vals[ex,], probs = seq(0, 1, length.out = n_knots+1)[2:n_knots]), degree = 3, intercept = TRUE)
    y_vals <- b_spline_basis %*% bspline_params[!is.na(bspline_params)]

    Mp[, "bspline"] <- approx(x_vals, y_vals, xout = valuesMatrix[, "bspline"])$y # Interpolation for Probabilities
  }



	if(report.elicited.p == F){
		Mp <- data.frame(Mp, row.names = values)}else{
		Mp <- data.frame(matrix(fit$probs[ex,], ncol=1), Mp, row.names = values)
		names(Mp) <- c("elicited", distribution.names)
	}

	if(report.elicited.q == F){
		Mq <- data.frame(Mq, row.names = quantiles)
		}else{
		Mq <- data.frame(fit$vals[ex,], Mq, row.names = quantiles)
		names(Mq) <- c("elicited", distribution.names)
	}

    # Highest Density Regions

    Lhdr <- vector("list", n.distributions)
    names(Lhdr) <- distribution.names

    # Normal distribution:
    if(!is.na(fit$ssq[ex, "normal"])){
          Lhdr[['normal']] <- hdrstd(qnorm,mean = fit$Normal[ex,1], sd = fit$Normal[ex,2],credMass = credMass)
	  }

    # t-distribution
    if(!is.na(fit$ssq[ex, "t"])){
          Lhdr[['t']] <- hdrstd(qt, df=fit$Student.t[ex,3], credMass = credMass) * fit$Student.t[ex,2] + fit$Student.t[ex,1]
	  }
    # skewnormal
    if(!is.na(fit$ssq[ex, "skewnormal"])){

         Lhdr[['skewnormal']]<-tryCatch(hdrstd(sn::qsn, xi = fit$Skewnormal[ex,1],omega = fit$Skewnormal[ex,2], alpha = fit$Skewnormal[ex,3],credMass = credMass),
                                        warning=function(w) {hdrstd(sn::qsn, xi = fit$Skewnormal[ex,1],omega = fit$Skewnormal[ex,2], alpha = fit$Skewnormal[ex,3],credMass = credMass, solver = "RFB")},
                                        error=function(w) {hdrstd(sn::qsn, xi = fit$Skewnormal[ex,1],omega = fit$Skewnormal[ex,2], alpha = fit$Skewnormal[ex,3],credMass = credMass, solver = "RFB")}
                                        )

      }
    # gamma
    if(!is.na(fit$ssq[ex, "gamma"])){
         Lhdr[['gamma']] <- hdrstd(qgamma, shape = fit$Gamma[ex,1], rate = fit$Gamma[ex,2],credMass = credMass) + fit$limits[ex,1]
    }

    # log-normal
    if(!is.na(fit$ssq[ex, "lognormal"])){
         Lhdr[['lognormal']] <- hdrstd(qlnorm, meanlog = fit$Log.normal[ex,1],sdlog = fit$Log.normal[ex,2],credMass = credMass) + fit$limits[ex,1]
    }

    # log-t
    if(!is.na(fit$ssq[ex, "logt"])){
      Lhdr[['logt']] <- hdrstd(qlogt, L = fit$limits[ex,1],mu = fit$Log.Student.t[ex, 1],sigma = fit$Log.Student.t[ex, 2],df = fit$Log.Student.t[ex,3],credMass = credMass)
    }

    # beta
    if(!is.na(fit$ssq[ex, "beta"])){
         Lhdr[['beta']] <- fit$limits[ex,1] + (fit$limits[ex,2] - fit$limits[ex,1]) * hdrstd(qbeta,shape1 = fit$Beta[ex,1],shape2 = fit$Beta[ex,2],credMass = credMass)
	  }

    # mirror-lognormal
    if(!is.na(fit$ssq[ex, "mirrorlognormal"])){
         Lhdr[["mirrorlognormal"]] <- hdrstd(
           function(p, U, meanlog, sdlog) {
             U - qlnorm(1 - p, meanlog = meanlog, sdlog = sdlog)
           },
           credMass = credMass,U= fit$limits[ex, 2],meanlog = fit$mirrorlognormal[ex, 1],sdlog   = fit$mirrorlognormal[ex, 2]
         )
	  }

     # mirror log-t
    if(!is.na(fit$ssq[ex, "mirrorlogt"])){
         Lhdr[["mirrorlogt"]] <- hdrstd(
           function(p, U, mu, sigma, df) {
             U - exp(qt(1 - p, df = df) * sigma + mu)
           },
           credMass = credMass, U = fit$limits[ex, 2], mu = fit$mirrorlogt[ex, 1], sigma = fit$mirrorlogt[ex, 2], df = fit$mirrorlogt[ex, 3]
         )
	  }

    # mirror gamma
    if(!is.na(fit$ssq[ex, "mirrorgamma"])){
         Lhdr[["mirrorgamma"]] <- hdrstd(
           function(p, U, shape, rate) {
             U - qgamma(1 - p, shape = shape, rate = rate)
           },
           credMass = credMass, U = fit$limits[ex, 2], shape = fit$mirrorgamma[ex, 1],rate  = fit$mirrorgamma[ex, 2]
         )
    }

    # Histogram
    if(fit$limits[ex,1] > - Inf & fit$limits[ex,2] < Inf) {
			Lhdr[["hist"]] <- hdrhist(hdr.prob/100,
			                 c(fit$limits[ex, "lower"], fit$vals[ex, ],
			                   fit$limits[ex, "upper"]),
			                 c(0, fit$probs[ex, ], 1 )
			                 )
		}

    ### Put results together
	 list(fitted.quantiles = round(Mq),
	      fitted.probabilities = signif(Mp, sf),
        fitted.hdr = Lhdr)
}

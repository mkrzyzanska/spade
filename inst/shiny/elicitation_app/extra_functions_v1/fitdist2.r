library(splines2)
library(splines)
library(scam)
fitdist2 <-
  function(vals, probs,cn, lower = -Inf,
           upper = Inf, weights = 1, tdf = 3,
           expertnames = NULL,
           excludelogt = FALSE,bs_degree=3,width=width,exponential_tails=exponential_tails,rtp=rtp,ltp=ltp,edr=edr){

##### Vals in the roulette are given by: rv <- bin.right()[rl$nonEmpty] (so this should be just the location of the bin?
##### Probs in the roulette are given by: rp <- rl$allBinsPr[rl$nonEmpty], with rl defined as:  rl$allBinsPr <- cumsum(rl$chips)/sum(rl$chips)
      
    
    if(is.matrix(vals)==F){vals<-matrix(vals, nrow = length(vals), ncol = 1)}
    if(is.matrix(probs)==F){probs <- matrix(probs, nrow = nrow(vals), ncol = ncol(vals))}
    
    
    if(is.matrix(weights)==F){weights <- matrix(weights, nrow = nrow(vals), ncol = ncol(vals))}
    if(length(lower)==1){lower <- rep(lower, ncol(vals))}
    if(length(upper)==1){upper <- rep(upper, ncol(vals))}
    if(length(tdf)==1){tdf <- rep(tdf, ncol(vals))}
    
    n.experts <- ncol(vals)
    normal.parameters <- matrix(NA, n.experts, 2)
    skewnormal.parameters <- matrix(NA, n.experts, 3)
    tParameters <- matrix(NA, n.experts, 3)
    mirrorgamma.parameters <- gamma.parameters <- 
      matrix(NA, n.experts, 2)
    mirrorlognormal.parameters <- 
      lognormal.parameters <- matrix(NA, n.experts, 2)
    mirrorlogt.parameters <- logt.parameters <-
      matrix(NA, n.experts, 3)
    beta.parameters <- matrix(NA, n.experts, 2)
    ssq<-matrix(NA, n.experts, 12)
    notes <- NULL
    
    colnames(ssq) <- c("normal", "t", "skewnormal",
                       "gamma", "lognormal", "logt", 
                      "beta", 
                       "mirrorgamma",
                       "mirrorlognormal",
                       "mirrorlogt","NS","MP")
    
    
    if(n.experts > 1 & n.experts < 27 & is.null(expertnames)){
      expertnames <- paste("expert.", LETTERS[1:n.experts], sep="")
    }
    
    if(n.experts > 27 & is.null(expertnames)){
      expertnames <- paste("expert.", 1:n.experts, sep="")
    }
    
    limits <- data.frame(lower = lower, upper = upper)
    row.names(limits) <- expertnames
    
    # requirements for distribution fitting ----
    
    for(i in 1:n.experts){
      if (length(probs[, i]) < 1){stop("need at least one elicited probability")}
      if (min(probs[,i]) < 0 | max(probs[,i]) > 1 ){stop("probabilities must be between 0 and 1")}
      if (min(vals[,i]) < lower[i]){stop("elicited parameter values cannot be smaller than lower parameter limit")}
      if (max(vals[,i]) > upper[i]){stop("elicited parameter values cannot be greater than upper parameter limit")}
      if (tdf[i] <= 0 ){stop("Student-t degrees of freedom must be greater than 0")}
      
      # Need to exclude any probability judgements
      # P(X<=x) = 0 or P(X<=x) = 1
      # Facilitator should enforce these probabilities via the parameter limits  
      
      inc <- (probs[, i] > 0) & (probs[, i] < 1)
      if(sum(inc) < 1){stop("need at least one probability between 0 and 1")}
      minprob <- min(probs[inc, i])
      maxprob <- max(probs[inc, i])
      minvals <- min(vals[inc, i])
      maxvals <- max(vals[inc, i])
      
      # Main distribution fits, assuming appropriately small and large probabilities elicited ----
      
       
      
      
      # Get starting values for optimisation ----
      # Fit a normal distribution to get starting values
      
      # Appropriately small and large probabilities specified:
      
      if ((min(probs[inc, i]) < 0.4 ) & (max(probs[inc, i]) > 0.6 )) {
        if (min(probs[-1,i] - probs[-nrow(probs),i]) < 0 ){stop("probabilities must be specified in ascending order")}
        if (min(vals[-1,i] - vals[-nrow(vals),i]) <= 0 ){stop("parameter values must be specified in ascending order")}
        
      
      q.fit <- approx(x = probs[inc,i], y = vals[inc,i],
                      xout = c(0.4, 0.5, 0.6))$y
      l <- q.fit[1] # estimated 40th percentile on original scale
      u <- q.fit[3] # estimated 60th percentile on original scale
      
     # if(minprob > 0 & maxprob < 1){
        
        minq <- qnorm(minprob)
        maxq <- qnorm(maxprob)
        # Estimate m and v assuming X~N(m,v)
        
        # Obtain m by solving simultaneously:
        # m + Z_l \sqrt{v} = X_l
        # m + Z_u \sqrt{v} = X_u
        # where Z_a is a-th quantile from N(0, 1), X_a is a-th quantile of X
        m <- (minvals * maxq - maxvals * minq) / (maxq - minq)
        v <- ((maxvals - minvals) / (maxq - minq))^2
        
        # mlog used for lognormal
        mlog <- (log(minvals - lower[i]) * 
                   maxq - log(maxvals - lower[i]) * minq) /
          (maxq - minq)
        
        # mlog used for mirror lognormal
        mlogMirror <- (log(upper[i] - maxvals) * 
                         (1 - minq) -
                         log(upper[i] - minvals) * (1-maxq)) /
          (maxq - minq)
        
     # }else{
      #  minq <- qnorm(min(probs[probs[, i] > 0, i]))
      #  maxq <- qnorm(max(probs[probs[, i] < 1, i]))
      #  m <- q.fit[2] # Estimated median on original scale
      #  v<- (u - l)^2 / 0.25 # Estimated variance on original scale
     # } 
      
      
      
       
      # Symmetric distribution fits ----
      
      normal.fit <- optim(c(m, 0.5*log(v)), 
                          normal.error, values = vals[inc,i], 
                          probabilities = probs[inc,i], 
                          weights = weights[inc,i])   
      normal.parameters[i,] <- c(normal.fit$par[1], exp(normal.fit$par[2]))
      ssq[i, "normal"] <- normal.fit$value
      
      # starting values: c(m, log((u - m)/ qt(0.6, tdf[i])))
      
      tFit <- optim(c(m, 0.5*log(v)), tError, 
                     values = vals[inc,i], 
                     probabilities = probs[inc,i], 
                     weights = weights[inc,i], 
                     degreesfreedom = tdf[i])
      tParameters[i, 1:2] <- c(tFit$par[1], exp(tFit$par[2]))
      tParameters[i, 3] <- tdf[i]
      ssq[i, "t"] <- tFit$value
      
      # skew normal fit ----
      # will fit 3 parameters in skew normal, so need at least 3 judgements
      if(length(vals[inc, i]) > 2){
        
        # Fit in two stages. First, optimise for location and scale, over
        # fixed grid of shape/slant parameters
        
        alphaVec <- c(-20, -10, -5:5, 10, 20)
        delta <- alphaVec / sqrt(1 + alphaVec^2)
        eVec <- rep(0, 15)
        
        # Get starting values by matching moments to fitted normal distribution
        omegaStart <- normal.parameters[i,2] / sqrt(1 - 2*delta^2/pi)
        xiStart <- normal.parameters[i,1] - omegaStart * delta*sqrt(2/pi)
        for(j in 1:15){
          
          eVec[j]<- optim(c(xiStart[j], log(omegaStart[j])), 
                          skewnormal.error, values = vals[inc, i], 
                          probabilities = probs[inc,i], 
                          weights = weights[inc,i],
                          snAlpha = alphaVec[j])$value

        }
        
        # Now find best fit, and optimise over all three parameters, starting
        # from that best fit
        
        index <- which.min(eVec)
        skewnormal.fit <- optim(c(xiStart[index], log(omegaStart[index]),
                                    alphaVec[index]), 
                                  skewnormal.error.joint,
                                values = vals[inc, i], 
                                  probabilities = probs[inc,i], 
                                  weights = weights[inc,i])
      
      skewnormal.parameters[i,] <- c(skewnormal.fit$par[1],
                                     exp(skewnormal.fit$par[2]),
                                     skewnormal.fit$par[3])
      ssq[i, "skewnormal"] <- skewnormal.fit$value
      
      }
      # Positive skew distribution fits ----
      
      
      if(lower[i] > -Inf){
        vals.scaled1 <- vals[inc,i] - lower[i]
        m.scaled1 <- m - lower[i]
        
        gamma.fit<-optim(c(log(m.scaled1^2/v), log(m.scaled1/v)), 
                         gamma.error, values = vals.scaled1, 
                         probabilities = probs[inc,i], 
                         weights = weights[inc,i])
        gamma.parameters[i,] <- exp(gamma.fit$par)
        ssq[i, "gamma"] <- gamma.fit$value
        
        std<-((log(u - lower[i])-log(l - lower[i]))/1.35)
        
        # mlog <- (log(minvals - lower[i]) * 
        #            maxq - log(maxvals - lower[i]) * minq) /
        #   (maxq - minq)
        
        lognormal.fit <- optim(c(mlog,
                                 log(std)), 
                               lognormal.error, 
                               values = vals.scaled1, 
                               probabilities = probs[inc,i], 
                               weights = weights[inc,i])
        lognormal.parameters[i, 1:2] <- c(lognormal.fit$par[1],
                                          exp(lognormal.fit$par[2]))
        ssq[i, "lognormal"] <- lognormal.fit$value
        
        logt.fit <- optim(c(log(m.scaled1), log(std)), 
                          logt.error, 
                          values = vals.scaled1, 
                          probabilities = probs[inc,i], 
                          weights = weights[inc,i], 
                          degreesfreedom = tdf[i])
        logt.parameters[i,1:2] <- c(logt.fit$par[1], exp(logt.fit$par[2]))
        logt.parameters[i,3] <- tdf[i]
        ssq[i, "logt"] <- logt.fit$value
      }
      
      # Beta distribution fits ----
      
      if((lower[i] > -Inf) & (upper[i] < Inf)){
        vals.scaled2 <- (vals[inc,i] - lower[i]) / (upper[i] - lower[i])
        m.scaled2 <- (m - lower[i]) / (upper[i] - lower[i])
        v.scaled2 <- v / (upper[i] - lower[i])^2
        
        alp <- abs(m.scaled2 ^3 / v.scaled2 * (1/m.scaled2-1) - m.scaled2)
        bet <- abs(alp/m.scaled2 - alp)
        if(identical(probs[inc, i], 
                     (vals[inc, i] - lower[i]) / (upper[i] - lower[i]))){
          alp <- bet <- 1
        }
        beta.fit <- optim(c(log(alp), log(bet)), 
                          beta.error, 
                          values = vals.scaled2, 
                          probabilities = probs[inc,i], 
                          weights = weights[inc,i])
        beta.parameters[i,] <- exp(beta.fit$par)
        ssq[i, "beta"] <- beta.fit$value	
        
      }
      
      # Negative skew distribution fits ----
      
      if(upper[i] < Inf){
        
        # Distributions are fitted to Y:= Upper limit - X
        
        valsMirrored <- upper[i] - vals[inc, i]
        probsMirrored <- 1 - probs[inc, i]
        mMirrored <- upper[i] - m
        
        # Mirror gamma
        
        
        
        mirrorgamma.fit<-optim(c(log(mMirrored^2/v), log(mMirrored/v)), 
                               gamma.error, values = valsMirrored, 
                               probabilities = probsMirrored, 
                               weights = weights[inc,i])
        mirrorgamma.parameters[i,] <- exp(mirrorgamma.fit$par)
        ssq[i, "mirrorgamma"] <- mirrorgamma.fit$value
        
        # Mirror log normal
        
        
        # Obtain mlogMirror by solving simultaneously:
        # m + Z_l \sqrt{v} = Y_l
        # m + Z_u \sqrt{v} = Y_u
        # where Z_a is a-th quantile from N(0, 1),
        # Y_a is a-th quantile of Y
        # and we model Y = log(upper - X) ~ N(mlogMirror, stdMirror^2)
        
        
        # mlogMirror <- (log(upper[i] - maxvals) * 
        #                  (1 - minq) -
        #                  log(upper[i] - minvals) * (1-maxq)) /
        #   (maxq - minq)
        
        stdMirror <-((log(upper[i] - l)-log(upper[i] - u))/1.35)
        
        
        mirrorlognormal.fit <- optim(c(mlogMirror,
                                       log(stdMirror)), 
                                     lognormal.error, 
                                     values = valsMirrored, 
                                     probabilities = probsMirrored, 
                                     weights = weights[inc,i])
        mirrorlognormal.parameters[i, 1:2] <-
          c(mirrorlognormal.fit$par[1],
            exp(mirrorlognormal.fit$par[2]))
        ssq[i, "mirrorlognormal"] <- mirrorlognormal.fit$value
        
        # Mirror log t
        
        mirrorlogt.fit <- optim(c(log(mMirrored), log(stdMirror)), 
                          logt.error, 
                          values = valsMirrored, 
                          probabilities = probsMirrored, 
                          weights = weights[inc,i], 
                          degreesfreedom = tdf[i])
        mirrorlogt.parameters[i,1:2] <- c(mirrorlogt.fit$par[1],
                                          exp(mirrorlogt.fit$par[2]))
        mirrorlogt.parameters[i,3] <- tdf[i]
        ssq[i, "mirrorlogt"] <- mirrorlogt.fit$value
        
      }
       }else{
         notes <- paste0("Did not have smallest elicited probability < 0.4 and > 0, ",
                         "and largest > 0.6 and < 1. If lower and/or upper limits specified, ",
                         "gamma and mirror gamma are fitted with the shape ",
                         "parameter fixed at 1, i.e. an exponential distribution.")
        
         # Exponential fit ----
         
         # If only a single probability specified, or probabilities 
         # too close to 0.5, will fit a gamma with shape parameter = 1
         # via fitting an exponential distribution
         
         if(lower[i] > -Inf){
           lambda <- -log(1 - maxprob)/(maxvals - lower[i])
           exponential.fit <- optimise(exponential.error,
                                       interval = c(0, 2 * lambda),
                                       values = vals[inc,i] - lower[i],
                                       probabilities = probs[inc,i], 
                                       weights = weights[inc,i])   
           gamma.parameters[i,] <- c(1, exponential.fit$minimum)
           ssq[i, "gamma"] <- exponential.fit$objective
           
         }
         
         if(upper[i] < Inf){
           lambda <- -log(minprob)/(upper[i] - minvals)
           mirrorexponential.fit <- optimise(exponential.error,
                                       interval = c(0, 2 * lambda),
                                       values = upper[i] - vals[inc, i],
                                       probabilities = 1 - probs[inc,i], 
                                       weights = weights[inc,i])   
           mirrorgamma.parameters[i,] <- c(1, mirrorexponential.fit$minimum)
           ssq[i, "mirrorgamma"] <- mirrorexponential.fit$objective
           
         }
          
       }
      
      
      
    }

  n.experts <- ncol(vals)
  n_coef_max <- 10 # Maximum number of coefficients we expect (adjust as needed)
  spline.parameters <- matrix(NA, nrow = 0, ncol = 0) # Initialize BEFORE the loop

  # ... inside the loop (for(i in 1:n.experts)) ...

      
    dfn <- data.frame(normal.parameters)
    names(dfn) <-c ("mean", "sd")
    row.names(dfn) <- expertnames
    
    dfsn <- data.frame(skewnormal.parameters)
    names(dfsn) <-c ("location", "scale" , "slant")
    row.names(dfsn) <- expertnames
    
    dft <- data.frame(tParameters)
    names(dft) <-c ("location", "scale", "df")
    row.names(dft) <- expertnames
    
    dfg <- data.frame(gamma.parameters)
    names(dfg) <-c ("shape", "rate")
    row.names(dfg) <- expertnames
    
    dfmirrorg <- data.frame(mirrorgamma.parameters)
    names(dfmirrorg) <-c ("shape", "rate")
    row.names(dfmirrorg) <- expertnames
    
    dfln <- data.frame(lognormal.parameters)
    names(dfln) <-c ("mean.log.X", "sd.log.X")
    row.names(dfln) <- expertnames
    
    dfmirrorln <- data.frame(mirrorlognormal.parameters)
    names(dfmirrorln) <-c ("mean.log.X", "sd.log.X")
    row.names(dfmirrorln) <- expertnames
    
    dflt <- data.frame(logt.parameters)
    names(dflt) <-c ("location.log.X", "scale.log.X", "df.log.X")
    row.names(dflt) <- expertnames
    
    dfmirrorlt <- data.frame(mirrorlogt.parameters)
    names(dfmirrorlt) <-c ("location.log.X", "scale.log.X", "df.log.X")
    row.names(dfmirrorlt) <- expertnames
    
    dfb <- data.frame(beta.parameters)
    names(dfb) <-c ("shape1", "shape2")
    row.names(dfb) <- expertnames
    
    ssq <- data.frame(ssq)
    row.names(ssq) <- expertnames
    
    if(excludelogt){
      reducedssq <- ssq[, c("normal", "t", "skewnormal", "gamma",
                              "lognormal", "beta", 
                              "mirrorgamma",
                              "mirrorlognormal")]
      index <- apply(reducedssq, 1, which.min)
      best.fitting <- data.frame(best.fit=
                                   names(reducedssq)[index])}else{
      index <- apply(ssq, 1, which.min)
      best.fitting <- data.frame(best.fit=names(ssq)[index])
      }
      
  
    
    row.names(best.fitting) <- expertnames

    ### Splines fit:

    NSsplineFit<-fitSplines(lower=lower,upper=upper,vals=vals[,1],probs=probs[,1],cn,degree=bs_degree,width=width,spline="NS",exponential_tails=exponential_tails,ltp=ltp,rtp=rtp,edr=edr)


      MPsplineFit <- tryCatch({
          
  # Call the fitSplines function here
          fitSplines(lower=lower,upper=upper,vals=vals[,1],probs=probs[,1],cn,degree=bs_degree,width=width,spline="MP",exponential_tails=exponential_tails,rtp=rtp,ltp=ltp,edr=edr)
}, error = function(e) {
  # Handle the error if it occurs
  print(paste("Error occurred in fitSplines:", e$message))
  return(NULL)  # Return NULL or handle the error as needed
})
#    MPsplineFit<-fitSplines(lower=lower,upper=upper,vals=vals[,1],probs=probs[,1],cn,degree=bs_degree,width=width,spline="MP",exponential_tails=exponential_tails,rtp=rtp,ltp=ltp,edr=edr)


    #####  
    vals <- data.frame(vals)
    names(vals) <- expertnames

    
    
    probs <- data.frame(probs)
    names(probs) <- expertnames
    fit <- list(Normal = dfn, Student.t = dft, Skewnormal = dfsn, 
                Gamma = dfg, Log.normal = dfln, 
                Log.Student.t = dflt, Beta = dfb,
                mirrorgamma = dfmirrorg,
                mirrorlognormal = dfmirrorln,
                mirrorlogt = dfmirrorlt,
                NS = NSsplineFit,
                MP = MPsplineFit,
                ssq = ssq, 
                best.fitting = best.fitting, vals = t(vals), 
                probs = t(probs), limits = limits, 
                notes = notes)
    class(fit) <- "elicitation"
    fit
  }
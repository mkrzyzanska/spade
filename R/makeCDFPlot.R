#' Plot the elicited cumulative probabilities 
#' 
#' Plots the elicited cumulative probabilities and, optionally,
#' a fitted CDF. Elicited are shown as filled circles, and
#' limits are shown as clear circles.
#'
#' @param lower lower limit for the uncertain quantity
#' @param v vector of values, for each value x in Pr(X<=x) = p
#' in the set of elicited probabilities
#' @param p vector of probabilities, for each value p in Pr(X<=x) = p
#' in the set of elicited probabilities
#' @param upper upper limit for the uncertain quantity
#' @param fontsize font size to be used in the plot
#' @param fit object of class \code{elicitation}
#' @param dist the fitted distribution to be plotted. Options are
#' \code{"normal"}, \code{"t"}, \code{"skewnormal"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"logt"},\code{"beta"}, \code{"mirrorgamma"},
#' \code{"mirrorlognormal"}, \code{"mirrorlogt"} \code{"hist"} (for a histogram fit)
#' @param showFittedCDF logical. Should a fitted distribution function
#' be displayed?
#' @param showQuantiles logical. Should quantiles from the fitted distribution function
#' be displayed?
#' @param ql a lower quantile to be displayed.
#' @param qu an upper quantile to be displayed.
#' @param ex if the object \code{fit} contains judgements from multiple experts,
#' which (single) expert's judgements to show.
#' @param sf number of significant figures to be displayed.
#' @param xaxisLower lower limit for the x-axis.
#' @param xaxisUpper upper limit for the x-axis.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' 
#' @examples
#' 
#' \dontrun{
#' vQuartiles <- c(30, 35, 45)
#' pQuartiles<- c(0.25, 0.5, 0.75)
#' myfit <- fitdist(vals = vQuartiles, probs = pQuartiles, lower = 0)
#' makeCDFPlot(lower = 0, v = vQuartiles, p = pQuartiles,
#'  upper = 100, fit = myfit, dist = "gamma",
#'  showFittedCDF = TRUE, showQuantiles = TRUE)
#' 
#' 
#' }
#'
#' @export

makeCDFPlot <- function(lower, v, p, upper, fontsize = 12,
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
                        ylab = expression(P(X<=x)),min_val=NULL,max_val=NULL){
  
  # Hack to avoid CRAN check NOTE
  
  x <- NULL
  

    xticks<-c(xaxisLower, xaxisUpper, v)

      print(paste("xticks",xticks))
      
      convert_year <- function(year) {
          if (year < 1) year-1 else year
      }

     new_ticks<-sapply(xticks,convert_year)

    
    
  
  p1 <- ggplot(data.frame(x = c(xaxisLower, xaxisUpper)), aes(x = x)) +
    annotate("point", x = v, y = p, size = 5) + 
    annotate("point", x = c(min_val, max_val), y = c(0, 1), size = 5, shape = 1)+
    labs(y = ylab, x = xlab) +
    scale_x_continuous(breaks = c(xaxisLower, xaxisUpper, v), labels=round(abs(new_ticks),1),
                       minor_breaks = NULL,
                       limits = c(xaxisLower, xaxisUpper)) +
    scale_y_continuous(breaks = c(0, 1, p),
                       minor_breaks = NULL) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = fontsize))

    if (all(new_ticks < 0)) {
       p1<-p1+ xlab("YEAR BCE")
      #  endDate=endDate+1
      #  startDate=startDate+1
      #  nBins=nBins-1
     } else if (all(new_ticks > 0)) {
        p1<-p1+ xlab("YEAR CE")
     } else {
    #    startDate=startDate+1
     #   nBins=nBins-1
        p1 <- p1 + 
        geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +  # Vertical line
       #  Annotate BCE (left side of x = 1)
             annotate("text", x = 1 - 0.3, y = 0.8, label = "BCE", color = "red", size = 6, hjust = 1) +
  
              # Annotate CE (right side of x = 1)
              annotate("text", x = 1 + 0.3, y = 0.8, label = "CE", color = "red", size = 6, hjust = 0) 
           p1<-p1+ xlab("YEAR")
     }
  
  
  # Add in CDF
  
  if(showFittedCDF){
    
    if(dist == "best"){
      dist <- fit$best.fitting[ex, 1]
    }
    
    
    if(dist == "hist"){
      dist.title <- "Histogram fit"
      p1 <- p1 + annotate("segment", x = c(min_val, v),
                            y = c(0, p),
                            xend = c(v, max_val),
                            yend = c(p, 1)) 
      if(showQuantiles){
        xl <- qhist(ql, c(lower, v, upper), c(0, p, 1))
        xu <- qhist(qu, c(lower, v, upper), c(0, p, 1))
          p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
                                  
    }
    if(dist == "normal"){
      if(is.na(fit$ssq[ex, "normal"])){
        dist.title <- "Normal distribution not fitted"
      }else{
      dist.title <- paste("Normal (mean = ",
                          signif(fit$Normal[ex,1], sf),
                          ", sd = ",
                          signif(fit$Normal[ex,2], sf), ")",
                          sep="")
      
      p1 <- p1 + stat_function(fun = pnorm, 
                             args = list(mean = fit$Normal[1, 1],
                                         sd = fit$Normal[1, 2])
                               )
      if(showQuantiles){
        xl <- qnorm(ql, mean = fit$Normal[1, 1], sd = fit$Normal[1, 2])
        xu <- qnorm(qu, mean = fit$Normal[1, 1], sd = fit$Normal[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
        }
      }
    }
    if(dist == "t"){
      if(is.na(fit$ssq[ex, "t"])){
        dist.title <- "Student-t distribution not fitted"
      }else{
      dist.title=paste("Student-t(",
                       signif(fit$Student.t[ex,1], sf),
                       ", ",
                       signif(fit$Student.t[ex,2], sf),
                       ")",
                       sep="")
      
      tcdf <- function(x){pt((x - fit$Student.t[1, 1]) /
                               fit$Student.t[1, 2], fit$Student.t[1, 3])}
      p1 <- p1 + stat_function(fun = tcdf)
      
      if(showQuantiles){
        xl <- fit$Student.t[1, 1] + 
          fit$Student.t[1, 2] * qt(ql, fit$Student.t[1, 3])
        xu <- fit$Student.t[1, 1] + 
          fit$Student.t[1, 2] * qt(qu, fit$Student.t[1, 3])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
    }
    
    if(dist == "skewnormal"){
      if(is.na(fit$ssq[ex, "skewnormal"])){
        dist.title <- "Skew normal distribution not fitted"
      }else{
        dist.title <- paste("Skew normal\n(location = ",
                            signif(fit$Skewnormal[ex,1], sf),
                            ", scale = ",
                            signif(fit$Skewnormal[ex,2], sf),
                            ", slant = ",
                            signif(fit$Skewnormal[ex,3], sf),")",
                            sep="")
        
        p1 <- p1 + stat_function(fun = sn::psn, 
                                 args = list(xi = fit$Skewnormal[1, 1],
                                             omega = fit$Skewnormal[1, 2],
                                             alpha = fit$Skewnormal[1, 3])
        )
        if(showQuantiles){
          xl <- sn::qsn(ql, xi = fit$Skewnormal[1, 1],
                        omega = fit$Skewnormal[1, 2],
                        alpha = fit$Skewnormal[1, 3])
          xu <- sn::qsn(qu, xi = fit$Skewnormal[1, 1],
                        omega = fit$Skewnormal[1, 2],
                        alpha = fit$Skewnormal[1, 3])
          p1 <- p1 + 
            addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
            addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
        }
      }
    }
    
    
    if(dist == "lognormal"){
      if(is.na(fit$ssq[ex, "lognormal"])){
        dist.title <- "Log normal distribution not fitted"
      }else{
      dist.title = paste("Log normal(",
                         signif(fit$Log.normal[ex,1], sf),
                         ", ",
                         signif(fit$Log.normal[ex,2], sf), ")",
                         sep="")
      
      lncdf <- function(x){
        plnorm(x - lower, 
               meanlog = fit$Log.normal[1, 1],
               sdlog = fit$Log.normal[1, 2])
        
      }
      p1 <- p1 + stat_function(fun = lncdf)
      
      if(showQuantiles){
        xl <- lower + qlnorm(ql, meanlog = fit$Log.normal[1, 1],
                             sdlog = fit$Log.normal[1, 2])
        xu <- lower + qlnorm(qu, meanlog = fit$Log.normal[1, 1],
                             sdlog = fit$Log.normal[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
      
    }
    
    if(dist == "mirrorlognormal"){
      if(is.na(fit$ssq[ex, "mirrorlognormal"])){
        dist.title <- "Mirror log normal distribution not fitted"
      }else{
      dist.title = paste("Mirror log normal(",
                         signif(fit$mirrorlognormal[ex,1], sf),
                         ", ",
                         signif(fit$mirrorlognormal[ex,2], sf), ")",
                         sep="")
      
      mirrorlncdf <- function(x){
        1- plnorm(upper - x, 
               meanlog = fit$mirrorlognormal[1, 1],
               sdlog = fit$mirrorlognormal[1, 2])
        
      }
      p1 <- p1 + stat_function(fun = mirrorlncdf)
      
      if(showQuantiles){
        xl <- upper - qlnorm(1 - ql, meanlog = fit$mirrorlognormal[1, 1],
                             sdlog = fit$mirrorlognormal[1, 2])
        xu <- upper -  qlnorm(1 - qu, meanlog = fit$mirrorlognormal[1, 1],
                             sdlog = fit$mirrorlognormal[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
      
    }
    
    if(dist == "gamma"){
      if(is.na(fit$ssq[ex, "gamma"])){
        dist.title <- "Gamma distribution not fitted"
      }else{
      dist.title = paste("Gamma(",
                         signif(fit$Gamma[ex,1], sf),
                         ", ",
                         signif(fit$Gamma[ex,2], sf),
                         ")", sep="")
      
      gcdf <- function(x){pgamma(x - lower, 
                                 shape = fit$Gamma[1, 1],
                                 rate = fit$Gamma[1, 2])}
      p1 <- p1 + stat_function(fun = gcdf)
      
      if(showQuantiles){
        xl <- lower + qgamma(ql,  
                             shape = fit$Gamma[1, 1],
                             rate = fit$Gamma[1, 2])
        xu <- lower + qgamma(qu,  
                             shape = fit$Gamma[1, 1],
                             rate = fit$Gamma[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
    }
    
    if(dist == "mirrorgamma"){
      if(is.na(fit$ssq[ex, "mirrorgamma"])){
        dist.title <- "Mirror gamma distribution not fitted"
      }else{
      dist.title = paste("Mirror gamma(",
                         signif(fit$mirrorgamma[ex,1], sf),
                         ", ",
                         signif(fit$mirrorgamma[ex,2], sf),
                         ")", sep="")
      
      mirrorgcdf <- function(x){1 - pgamma(upper - x, 
                                 shape = fit$mirrorgamma[1, 1],
                                 rate = fit$mirrorgamma[1, 2])}
      p1 <- p1 + stat_function(fun = mirrorgcdf)
      
      if(showQuantiles){
        xl <- upper - qgamma(1 - ql,  
                             shape = fit$mirrorgamma[1, 1],
                             rate = fit$mirrorgamma[1, 2])
        xu <- upper - qgamma(1 - qu,  
                             shape = fit$mirrorgamma[1, 1],
                             rate = fit$mirrorgamma[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      }
    }
    
    if(dist == "logt"){
      if(is.na(fit$ssq[ex, "logt"])){
        dist.title <- "Log Student-t distribution not fitted"
      }else{
      dist.title = paste("Log T(",
                         signif(fit$Log.Student.t[ex,1], sf),
                         ", ",
                         signif(fit$Log.Student.t[ex,2], sf),
                         ")", sep="")
      
      lntcdf <- function(x){
        # Need to handle case of x < lower
        
        p <- pt((log(abs(x - lower)) - fit$Log.Student.t[1, 1]) /
                  fit$Log.Student.t[1, 2], 
                fit$Log.Student.t[1, 3])
        p[x <= lower] <- 0
        p
      }
      p1 <- p1 + stat_function(fun = lntcdf)
      
      if(showQuantiles){
        xl <- lower + exp(fit$Log.Student.t[1, 1] + 
                            fit$Log.Student.t[1, 2] * 
                            qt(ql, fit$Log.Student.t[1, 3]))
        xu <- lower + exp(fit$Log.Student.t[1, 1] + 
                            fit$Log.Student.t[1, 2] * 
                            qt(qu, fit$Log.Student.t[1, 3]))
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      
    }
    }
    if(dist == "mirrorlogt"){
      if(is.na(fit$ssq[ex, "mirrorlogt"])){
        dist.title <- "Mirror log Student-t distribution not fitted"
      }else{
      dist.title = paste("Mirror log T(",
                         signif(fit$mirrorlogt[ex,1], sf),
                         ", ",
                         signif(fit$mirrorlogt[ex,2], sf),
                         ")", sep="")
      
      mirrorlntcdf <- function(x){
        # Need to handle case of x > upper
        
        p <- 1 - pt((log(abs(upper - x)) - fit$mirrorlogt[1, 1]) /
                  fit$mirrorlogt[1, 2], 
                fit$mirrorlogt[1, 3])
        p[x >= upper] <- 1
        p
      }
      p1 <- p1 + stat_function(fun = mirrorlntcdf)
      
      if(showQuantiles){
        xl <- upper -  exp(fit$mirrorlogt[1, 1] + 
                            fit$mirrorlogt[1, 2] * 
                            qt(1-ql, fit$mirrorlogt[1, 3]))
        xu <- upper -  exp(fit$mirrorlogt[1, 1] + 
                            fit$mirrorlogt[1, 2] * 
                            qt(1 - qu, fit$mirrorlogt[1, 3]))
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      
    }
    }
      
    if(dist == "beta"){
      
      if(is.na(fit$ssq[ex, "beta"])){
        dist.title <- "Beta distribution not fitted"
      }else{
      
      dist.title =paste("Beta(",
                        signif(fit$Beta[ex,1], sf),
                        ", ", signif(fit$Beta[ex,2], sf),
                        ")", sep="")
      
     
      bcdf <- function(x){pbeta((x - lower) / (upper - lower), 
                                 shape1 = fit$Beta[1, 1],
                                 shape2 = fit$Beta[1, 2])}
      p1 <- p1 + stat_function(fun = bcdf)
      
      if(showQuantiles){
        xl <- lower + (upper - lower) * qbeta(ql, 
                                              shape1 = fit$Beta[1, 1],
                                              shape2 = fit$Beta[1, 2])
        xu <- lower + (upper - lower) * qbeta(qu, 
                                              shape1 = fit$Beta[1, 1],
                                              shape2 = fit$Beta[1, 2])
        p1 <- p1 + 
          addQuantileCDF(xaxisLower, xl, ql, xaxisUpper) + 
          addQuantileCDF(xaxisLower, xu, qu, xaxisUpper) 
      }
      
      }
    }

    if(dist == "NS"){
        x<-fit$NS$values
        fx<-fit$NS$cdf
        ns_df<- data.frame(x=x,fx=fx)
        dist.title="Natural Cubic Spline"
        extra_points<-fit$NS$extra_points
        if(length(extra_points)>0){
        p1 <- p1 + geom_point(data=extra_points,aes(x=x,y=y),col="red",inherit.aes=FALSE,size=2,alpha=0.5)

        }
        p1<-p1 + geom_line(data = ns_df, aes(x = x, y = fx),inherit.aes=FALSE)
        
    }

     if(dist == "MP"){
        x<-fit$MP$values
        fx<-fit$MP$cdf
        mp_df<- data.frame(x=x,fx=fx)
        dist.title="Monotonic p-spline"
        extra_points<-fit$MP$extra_points
        if(length(extra_points)>0){
        p1 <- p1 + geom_point(data=extra_points,aes(x=x,y=y),col="red",inherit.aes=FALSE,size=2,alpha=0.5)

        }
        p1<-p1 + geom_line(data = mp_df, aes(x = x, y = fx),inherit.aes=FALSE)
    }
   
      
      
  p1 <- p1 + labs(title = dist.title)  
  }
 
  
  p1 
  
 }

feedback <- function(fit, quantiles =  NA, values = NA, dist= "best", ex = NA, sf = 3,hdr.prob=10){
  if(nrow(fit$vals)>1 & is.na(ex)==T){
    return(feedbackgroup(fit, quantiles, values, dist, sfg = sf))
  }
  
  if(nrow(fit$vals)>1 & is.na(ex)==F){
    return(feedbacksingle(fit, quantiles, values, sf, ex,hdr.prob))
  }
  
  if(nrow(fit$vals)==1){
    return(feedbacksingle(fit, quantiles, values, sf,hdr.prob=hdr.prob))
  }
}
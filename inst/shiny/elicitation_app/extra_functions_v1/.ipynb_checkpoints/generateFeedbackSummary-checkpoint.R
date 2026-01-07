generateFeedbackSummary<- function(fit, dist,fqc,fpc){

      ### Quantile Values:  
      FB <- feedback(fit,
                       quantiles = fqc,
                       ex = 1)
        
        if(dist != "best"){
           values <- FB$fitted.quantiles[, dist]
        }else{
         
            values <- FB$fitted.quantiles[, 
                                        as.character(fit$best.fitting[1,
                                                                          1])]
          
        }
       
        df.values<-data.frame(Values=values,Probabilities=fqc)

      ### Probabilities:

      FB <- feedback(fit, 
                       values = fpc,
                       ex = 1)
        if(dist != "best"){
             probs <- FB$fitted.probabilities[, dist]
          
        }else{
          probs <- FB$fitted.probabilities[, 
                                        as.character(fit$best.fitting[1,
                                                                          1])]
        }
      
      df.probs<-data.frame(Probabilities=probs,Values=fpc)

      feedback_summary<-list(values = df.values, probs = df.probs)
      
      return(feedback_summary)
    
      }
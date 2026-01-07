feedbackText<-function(feedback_summary){
    values <- feedback_summary$values$Values
    probs<-feedback_summary$probs$Probabilities
    pvals<-feedback_summary$probs$Values

    
    
    # Check if values are NULL and handle that case
    if (is.null(values)) {
        return("Quantiles are out of bounds or not available.")
    }
    
    # Create a formatted text output

    values <- sapply(values,function(x){
        if(x<1){
           text<- paste(abs(x-1),"BCE")}
       else{text <- paste(x,"CE")}
        return(text)
        })

      pvals <- sapply(pvals,function(x){
        if(x<1){
           text<- paste(round(abs(x-1),0),"BCE")}
       else{text <- paste(x,"CE")}
        return(text)
        })
    s1<- paste("There is about 10% probability that the artefact was deposited before", values[1], sep=" ")
    s2<-paste("There is about 10% probability that the artefact was deposited after", values[2],sep=" ")
    s3<-paste("It is equally likely that the artefact was deposited before and after", values[3],sep=" ")
    s4<-paste("There is about ",round(probs[1] * 100,2), "% probabiliy that the artefact was deposited before ", pvals[1] ,sep="")
    s5<-paste("There is about ",round((1-probs[2]) * 100,2), "% probabiliy that the artefact was deposited after ", pvals[2],sep="")
    HTML(paste0("<ul>",
              "<li>", s1, "</li>",
              "<li>", s2, "</li>",
              "<li>", s3, "</li>",
             "<li>", s4, "</li>",
             "<li>", s5, "</li>",
              "</ul>"))
}
    
to.BCECE <- function (x, toText=FALSE) {
  x<-ifelse(x < 0, x+1, x)
  if(toText==TRUE){
  x<-ifelse(x<1,paste(abs(round(x)),"BCE"), paste(round(x),"CE"))
  }
  return(x)
}

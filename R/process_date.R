#' Title
#'
#' @param date
#'
#' @returns
#' @export
#'
#' @examples
process_date <- function(date){
  date$rl <-list()
  date$rl$chips<-date$chips
  date$rl$allBinsPr <- cumsum(date$rl$chips)/sum(date$rl$chips)
  date$rl$nonEmpty <- date$rl$allBinsPr > 0 & date$rl$allBinsPr < 1
  date$bin.width <- ((date$endDate-date$startDate)/date$nBins)
  date$bin.left <- seq(from = date$startDate,
                       to = date$endDate - date$bin.width,
                       length=date$nBins)
  date$bin.right <- seq(from = date$startDate + date$bin.width,
                        to = date$endDate,
                        length = date$nBins)
  date$p <- date$rl$allBinsPr[date$rl$nonEmpty]
  date$v <- date$bin.right[date$rl$nonEmpty]
  return(date)
}

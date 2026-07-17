#' Title
#'
#' @param dates_list
#'
#' @returns
#' @export
#'
#' @examples
CalDatesTransform <- function(dates, grid_length=200, common_grid=FALSE){
  df <- data.frame(matrix(ncol = 12, nrow = length(dates)))
  colnames(df)<-c('DateID','CRA','Error','Details','CalCurve','ResOffsets','ResErrors','StartBP','EndBP','Normalised','F14C','CalEPS')
  df_dates <- lapply(dates, function(dates_list) {
    dates_list$chips <- NULL
    return(as.data.frame(dates_list, stringsAsFactors = FALSE))
  })
  keep_cols <- c("QoI", "date", "expert", "findtype", "startDate", "endDate", "selected_distribution")
  # 2. Extract only those columns and convert each entry to a single-row dataframe
  df_list <- lapply(dates, function(dates_list) {
    as.data.frame(dates_list[c("UFI","selected_distribution")], stringsAsFactors = FALSE) })
  final_df <- do.call(rbind, df_list)
  df$DateID <- final_df$UFI
  x_final<-list()
  x_final$metadata<-df

  x_final$grids<-list()

  if (common_grid) {
    # Calculate global timeline boundaries for the pool using the specified length
    global_start <- min(sapply(dates, function(d) d$startDate))
    global_end   <- max(sapply(dates, function(d) d$endDate))
    master_timeline <- seq(from = global_start, to = global_end, length.out = grid_length)
  }

  for (i in 1:length(dates)){
    date<-dates[[i]]
    print(date$selected_distribution)
    dist=date$selected_distribution
    xl = date$startDate
    xu = date$endDate

    if (common_grid==TRUE) {
      x <- master_timeline
    } else {
      # Customizes the individual grid length cleanly
      x <- seq(from = xl, to = xu, length.out = grid_length)
    }
    if (dist=="gamma"){
      fx <- ifelse(x > xl, dgamma(x - xl, date$shape, date$rate), 0)
    }else if(dist=="logt"){
      fx <- ifelse(x - xl > 0,
                   dt((log(x - xl) - date$location.log.X) / date$scale.log.X, date$df.log.X) / ((x - xl) * date$scale.log.X),
                   0)
    }else if(dist=="skewnormal"){
      fx <- sn::dsn(x, date$location,date$scale , date$slant)
    }

    x_final$grids[[i]] <- data.frame("calBP"=1950-x,"PrDens"=fx)
  }
  x_final$calmatrix <- NA
  class(x_final)<-c("CalDates","list")
  return((x_final))
}

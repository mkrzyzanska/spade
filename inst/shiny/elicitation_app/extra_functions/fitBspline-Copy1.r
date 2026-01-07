fitBspline <- function(lower, upper, vals, probs,degree,cn,width){
    print(paste("Cn:",cn))
# Adjust the cumulative probabilities to account for the limits
    extended_vals <- c(lower, vals, upper)
    extended_cumsums <- c(0, probs, 1)

    print("b")
    print(paste("x<-c(",paste(unlist(extended_vals),collapse=","),")",sep=""))
    print(paste("y<-c(",paste(unlist(extended_cumsums),collapse=","),")",sep=""))

    cdf_bspline_fit <- lm(extended_cumsums ~ bSpline(extended_vals, degree = degree))

    # Show the summary of the fitted B-spline model
    #print(summary(cdf_bspline_fit))
   

   # Calculate the derivative of the fitted CDF (PDF)
    cdf_bspline_derivative <- diff(predict(cdf_bspline_fit)) / diff(extended_vals)

    # Extend the derivative to match the entire range including the last point (upper limit)
    extended_pdf_vals <- c(0, cdf_bspline_derivative)  # Add the first value at the start
    #extended_vals_pdf <- seq(lower, upper, length.out = length(extended_pdf_vals))

    # Plot the PDF derived from the derivative
    #lines(extended_vals_pdf, extended_pdf_vals, col = "green", lwd = 2)

    # Compute the integral of the PDF (sum of areas)
    pdf_integral <- sum(extended_pdf_vals) * (upper - lower) / length(extended_pdf_vals)

    # Normalize the PDF (make sure the integral is 1)
    pdf_normalized <- extended_pdf_vals / pdf_integral
    
    #b_spline <- list(fit.cdf=cdf_bspline_fit,pdf_normalized=pdf_normalized,values=extended_vals)
    #print(b_spline)
    #return(b_spline)


    #Alternative fitting to pdf:
    mid_points <- c(vals-(width/2),max(vals)+(width/2))
    left_points<- c(vals-width,max(vals))
    #extended_vals <- c(lower, mid_points, upper)
    #extended_freq <- c(0, cn, 0)

    # Fit left and mid points (original options)
    #extended_vals <- c(lower, left_points, mid_points,vals,max(vals)+width, upper)
    #extended_freq <- c(0, cn,cn,cn, 0)

    # Fit all corners, but without midpoints:
    # lower - is the lower absolute limit
    # upper - is the upper absolute limit

    # vals - values to the right
    # 

    #extended_vals <- c(lower, left_points, mid_points,vals,max(vals), upper)
    #extended_freq <- c(0, cn,cn,cn, 0)

    #extended_vals <- c(lower, left_points,vals,max(vals)+width, upper)
    #extended_freq <- c(0, cn,cn, 0)


    extended_vals <- c(lower, c(rbind(left_points,c(vals,max(vals)+width))), upper)
    extended_freq <- c(0, c(rbind(cn,cn)), 0)
    
  
    print("a")
    print(paste("vals<-c(",paste(unlist(vals),collapse=","),")",sep=""))
    print(paste("x<-c(",paste(unlist(extended_vals),collapse=","),")",sep=""))
    print(paste("y<-c(",paste(unlist(extended_freq),collapse=","),")",sep=""))
    
    
    #print(paste("mid_points:",length(extended_vals)))
    #print(extended_freq)
    pdf_bspline_fit <- lm(extended_freq ~ bSpline(extended_vals, degree = degree))
    #print(summary(pdf_bspline_fit))
    pdf_values <- predict(pdf_bspline_fit)
    # Compute the integral of the PDF (sum of areas)
    pdf_integral <- sum(pdf_values) * (upper - lower) / length(pdf_values)
    # Normalize the PDF (make sure the integral is 1)
    pdf_normalized <- pdf_values / pdf_integral


    #Make predictions for new points:

    # Create a fine sequence of x-values for a smooth curve
    smooth_x <- seq(min(extended_vals), max(extended_vals), length.out = 100)  # 100 points
  

    smooth_pdf_values <- predict(pdf_bspline_fit, newdata = data.frame(extended_vals=smooth_x))


   # Compute the integral (sum of areas)
    pdf_integral <- sum(smooth_pdf_values) * (max(extended_vals) - min(extended_vals)) / length(smooth_pdf_values)

    # Normalize the PDF values
    smooth_pdf_normalized <- smooth_pdf_values / pdf_integral
    
    print(paste("smooth_x<-c(",paste(unlist(smooth_x),collapse=","),")",sep=""))
    print(paste("smooth_pdf_normalised<-c(",paste(unlist(smooth_pdf_normalized),collapse=","),")",sep=""))

    #print(paste("pdf_normalized:",length(pdf_normalized)))
    #b_spline <- list(fit.cdf=pdf_bspline_fit,pdf_normalized=pdf_normalized,values=extended_vals)
    b_spline <- list(fit.cdf=pdf_bspline_fit,pdf_normalized=smooth_pdf_values,values=smooth_x)
        return(b_spline)
    
}
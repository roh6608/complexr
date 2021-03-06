#' Convert degrees to radians
#'
#' @param x The value in degrees to be converted to radians.
#' @return The value in radians.
#' @examples
#' deg_rad(90)
deg_rad = function(x){
  if(is.numeric(x) != T){
    stop("Wrong type passed to x, please pass values of numeric type.")
  } else{
    return(x*pi/180)
  }
}

#' Convert radians to degrees
#'
#' @param x The value in radians to be converted to degrees.
#' @return The value in degrees
#' @examples
#' rad_deg(pi/2)
rad_deg = function(x){
  if(is.numeric(x) != T){
    stop("Wrong type passed to x, please pass values of numeric type.")
  } else{
    return(x*180/pi)
  }
}

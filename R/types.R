#' Convert from polar to rectangular form
#'
#' @param mod The modulus of the complex number, or vector of moduli.
#' @param arg The argument of the complex number or vector of arguments.
#' @param ang The units for the argument of the complex number, can either be rad or deg, defaults to rad.
#' @return The rectangular form of the complex number.
#' @examples
#' polar_rect(1,pi/4,"rad")
polar_rect = function(mod, arg, ang = "rad"){
  if((is.vector(mod) == T) && (is.vector(arg) == T) && (is.numeric(mod) == T) && (is.numeric(arg))){
    if(length(mod) != length(arg)){
      stop("Vector length mis-match.")
    } else{
      if(ang == "rad"){
        x = mod*cos(arg)
        y = mod*sin(arg)

      } else if(ang == "deg"){
        x = mod*cos(deg_rad(arg))
        y = mod*sin(deg_rad(arg))

      } else{
        warning("Valid string for ang was not given, defaulting to radians.")
        x = mod*cos(arg)
        y = mod*sin(arg)
      }
    }
  } else if((is.numeric(mod) == T) && (is.numeric(arg) == T)){
    if(ang == "rad"){
      x = mod*cos(arg)
      y = mod*sin(arg)

    } else if(ang == "deg"){
      x = mod*cos(deg_rad(arg))
      y = mod*sin(deg_rad(arg))

    } else{
      warning("Valid string for ang was not given, defaulting to radians")
      x = mod*cos(arg)
      y = mod*sin(arg)
    }
  } else{
    stop("Wrong type passed to mod and/or arg, please pass values of numeric type.")
  }



  return(complex(real = x, imaginary = y))
}

#' Convert from rectangular to polar form
#'
#' @param z A complex number or vector of complex numbers in rectangular form.
#' @param ang The units for the output, can either be rad or deg, defaults to rad.
#' @return The polar form of the complex number as a list, note the units are as specified by ang.
#' @examples
#' rect_polar(1+1i, "deg")
rect_polar = function(z, ang = "rad"){
  if(ang == "rad"){
    mod = Mod(z)
    arg = Arg(z)
    return(list(mod, arg))

  } else if(ang == "deg"){
    mod = Mod(z)
    arg = Arg(z)
    return(list(mod, rad_deg(arg)))
  } else{
    warning("Valid string for ang was not given, defaulting to radians")
    mod = Mod(z)
    arg = Arg(z)
    return(list(mod, arg))
  }

}

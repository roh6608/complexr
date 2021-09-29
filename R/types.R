#' Convert from polar to rectangular form
#'
#' @param mod The modulus of the complex number, or vector of moduli.
#' @param arg The argument of the complex number or vector of arguments, in radians.
#' @return The rectangular form of the complex number.
#' @examples
#' polar_rect(1,pi/4)
polar_rect = function(mod, arg){
  x = mod*cos(arg)
  y = mod*sin(arg)

  return(complex(real = x, imaginary = y))
}

#' Convert from rectangular to polar form
#'
#' @param z A complex number or vector of complex numbers in rectangular form.
#' @return The polar form of the complex number as a list, note the argument is in radians.
#' @examples
#' rect_polar(1+1i)
rect_polar = function(z){
  mod = Mod(z)
  arg = Arg(z)
  return(list(mod, arg))
}

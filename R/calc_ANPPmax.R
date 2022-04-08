#' @title Maximum Above-ground Net Primary Production
#'
#' @description This function calculates the max ANPP  given mean annual precipitation and temperature.
#' @param RAIN Long-term Mean Annual Precipitation (mm/year).
#' @param MAT Long-term Mean Annual Precipitation (degrees Celsius).
#' @export


calc_ANPPmax = function(RAIN, MAT) {

  ANPPmax = 12.04-25.18/(0.0083*(MAT+273.15))+0.72*log(RAIN)
  return(ANPPmax)

}

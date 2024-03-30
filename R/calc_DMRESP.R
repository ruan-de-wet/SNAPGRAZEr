#' @title Daily microbial respiration
#'
#' @description Calculation of the daily microbial respiration (gC/m2/year), from Ritchie (2020). This equation has been determined by a stepwise function, as determined by lab incubations of Serengeti soils.
#' @param SOCt Soil organic carbon stocks in year t (gC/m2).
#' @param lowSOC Default = FALSE. Different regression equation for respiration rate is applied for low and high SOC to avoid a negative respiration rate (which isn't physically possible). Threshold for what qualifies as "low SOC" is 4,600 gC/m^2 (i.e. 46 t/ha). Low SOC regression equation is applicable for higher SOC, but just with slightly lower R-squared.
#' @export

calc_DMRESP = function(SOCt, lowSOC = FALSE) {

  if(lowSOC) {

    DMRESP = -0.579+0.000358*SOCt

  } else {

    DMRESP = exp(-10.872)*SOCt^(1.296)

  }

  return(DMRESP)

}

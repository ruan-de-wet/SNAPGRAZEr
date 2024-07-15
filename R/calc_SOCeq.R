#' @title Equilibrium SOC
#'
#' @description To calculate the change in SOC for year t (deltaSOCt), we need the combination of PDSOCt and DDSOCt, but also the maximum rate of microbial respiration for year t (MRESPt). A key input to MRESPt is WETDAYS, which is calculated as part of this function. Equilibrium SOC can be calculated by solving for deltaSOC = 0, which then avoids the need for SOC measurement.
#' @param PDSOCt Output of calc_PDSOCt()
#' @param DDSOCt Output of calc_DDSOCt()
#' @param SAND Sand % in top 30 cm soil
#' @param RAIN MAP for year t (mm/year)
#' @param Gdays Total number of days in the growing season.
#' @param lowSOC Default = FALSE. Different regression equation for respiration rate is applied for low and high SOC to avoid a negative respiration rate (which isn't physically possible). Threshold for what qualifies as "low SOC" is 4,600 gC/m^2 (i.e. 46 t/ha). Low SOC regression equation is applicable for higher SOC, but just with slightly lower R-squared.
#' @param c1 Coefficient 1. Multiplied by SOCt in the calculation of DMRESP when lowSOC = TRUE.
#' @param c2 Coefficient 2. Multiplied by RAIN in the calculation of WETDAYS.
#' @export

calc_SOCeq = function(PDSOCt, DDSOCt, SAND, RAIN, Gdays, lowSOC = FALSE, c1 = 0.000358, c2 = 0.00044) {

  WETDAYS = (c2*RAIN-0.025)*Gdays

  if(lowSOC) {

    SOCeq = ((PDSOCt+DDSOCt)/(WETDAYS*(0.7+0.3*(SAND/100))*exp(-10.872)))^(1/1.296)

  } else {

    SOCeq = ((PDSOCt+DDSOCt+0.579*WETDAYS*(0.7+0.3*(SAND/100)))/(c1*WETDAYS*(0.7+0.3*(SAND/100))))+0.579

  }

  return(SOCeq)

}


#' @title Estimated Below-ground Net Primary Production
#'
#' @description This function calculates the estimated BNPP given mean annual precipitation, temperature, grazing management and the dominance of annual or perennial grasses.
#' @param RAIN Long-term Mean Annual Precipitation (mm/year).
#' @param MAT Long-term Mean Annual Precipitation (degrees Celsius).
#' @param ANPP_est (equivalent to Pg) Total seasonal grass production for the given grazing management system. Output of calc_ANPPest() or calc_Pg().
#' @param Sk The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.
#' @param S0 Biomass condition prior to the growing season (at the end of the dry season) that is mostly comprised of carbon stores in rhizomes. Default is 0.1*SK.
#' @param APCcorrection Default = FALSE. A correction factor for the influence of annual versus perennial plant growth strategies on belowground production. If correction is applied, then APC = 0.291. This is where forage is dominated by annuals (or shrubs often associated with annuals in drier grasslands). Otherwise, APC = 1.
#' @param DEPTH Default = 30. Depth of soil sampling / estimation (cm). The original SNAP model was developed based on measurements to a depth of 40 cm.
#' @export

calc_BNPPest = function(RAIN, MAT, ANPP_est, Sk, S0 = 0.1*Sk, APCcorrection = FALSE, DEPTH = 30) {

  APC = ifelse(APCcorrection, 0.291, 1)
  BNPPest = (0.602*RAIN-0.00038*RAIN^2+5.88*MAT)*(ANPP_est/(Sk-S0))*APC*(DEPTH/40)
  return(BNPPest)

}

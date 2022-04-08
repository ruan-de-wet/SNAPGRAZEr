#' @title Biomass at the start of grazing episode
#'
#' @description Calculation of the parameter Se, the biomass at the start of grazing episode, from Ritchie (2020)
#' @param Sk The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.
#' @param S0 The initial biomass condition prior to the growing season (at the end of the dry season) that is mostly comprised of carbon stores in rhizomes. Default is 0.1*SK.
#' @param Edays Number of days within the growing season prior to grazing episode
#' @param r Relative growth rate of grass biomass, which is variable over the growing season given it's density dependence. Can be calculated as tht intercept of a relationship between the measured relative growth rate and biomass at a given time. Default for tropical grasslands is 0.05 and for temperate grasslands is 0.035.
#' @export

calc_SE = function(Sk, Edays, S0 = 0.1*Sk, r = 0.05) {

  Se = Sk*S0/(Sk*exp(-r*Edays)+S0*(1-exp(-r*Edays)))
  return(Se)

}

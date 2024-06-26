#' @title Biomass at the end of growing season
#'
#' @description Calculation of the parameter Sf, the biomass at the end of growing season, from Ritchie (2020)
#' @param Sk The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.
#' @param Sg Biomass at the end of grazing episode. Output of calc_Sg.
#' @param r Relative growth rate of grass biomass, which is variable over the growing season given it's density dependence. Can be calculated as tht intercept of a relationship between the measured relative growth rate and biomass at a given time. Default for tropical grasslands is 0.05 and for temperate grasslands is 0.035.
#' @param Fdays Number of days left in the growing season after the grazing episode. Fdays = Gdays - Edays - Ddays
#' @export

calc_Sf = function(Sk, Sg, r, Fdays) {

  Sf = (Sk*Sg)/(Sk*exp(-(r*Fdays))+Sg*(1-exp(-(r*Fdays))))
  return(Sf)

}


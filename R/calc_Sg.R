#' @title Biomass at the end of grazing episode
#'
#' @description Calculation of the parameter Sg, the biomass at the end of grazing episode, from Ritchie (2020)
#' @param Sk The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.
#' @param Se Biomass at the start of grazing episode. Output of calc_Se.
#' @param Lg Biomass loss during grazing episode. Output of calc_Lg.
#' @param Ddays Number of days of grazing episode
#' @param n Number of "pastures" per total area, A.
#' @param d Stocking density (head/ha)
#' @param r Relative growth rate of grass biomass, which is variable over the growing season given it's density dependence. Can be calculated as tht intercept of a relationship between the measured relative growth rate and biomass at a given time. Default for tropical grasslands is 0.05 and for temperate grasslands is 0.035.
#' @param W Average animal body size (kg live weight)
#' @param Cg Daily consumption rate (g/animal/day)
#' @export

calc_Sg = function(Sk, Se, Lg, Ddays, n, d, r, W = NA, Cg = NA) {

  if(is.numeric(Cg)) {

    Cg = Cg

  } else if(is.numeric(W)) {

    Cg = 2*(5300+770*log(W))

  } else {print("You need to either provide Cg (daily consumption) or W (animal body size)")}

  g = (d*Cg*n*10^(-4))/Se
  Sg = (Sk*Se)/(Sk*exp(-(r-g)*Ddays)+Se*(1-exp(-(r-g)*Ddays)))-Lg
  return(Sg)

}


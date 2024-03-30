#' @title Maximum sustainable stocking density
#'
#' @description Calculation of the theoretical maximum stocking density, from Ritchie (2020). This is based on the assumptions about the consumption rate in the non-growing season relative to the available biomass.
#' @param Sf Biomass at the end of the growing season. Output of calc_Sf.
#' @param Sk The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.
#' @param Cg Daily consumption rate (g/animal/day)
#' @param Gdays Total number of days in the growing season.
#' @export

calc_dmax = function(Sf, Sk, Cg, Gdays) {

  dmax = ((Sf-0.1*Sk)*10^4)/((Cg/2)/365-Gdays)
  return(dmax)

}

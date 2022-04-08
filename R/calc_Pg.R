#' @title Total seasonal grass production
#'
#' @description Calculation of the parameter Pg, the total seasonal grass production for the given grazing management system, from Ritchie (2020)
#' @param Se Biomass at the start of the grazing episode. Output of calc_Se.
#' @param Sg Biomass at the end of grazing episode. Output of calc_Sg.
#' @param Sf Biomass at the end of the growing season. Output of calc_Sf.
#' @param Sk The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.
#' @param S0 Biomass condition prior to the growing season (at the end of the dry season) that is mostly comprised of carbon stores in rhizomes. Default is 0.1*SK.
#' @export

calc_Pg = function(Se, Sg, Sf, Sk, S0 = 0.1*Sk) {

  if(Sg < Se) {
    Pg = (Se-S0) + (Sf-Sg)
  } else {
    Pg = (Se-S0) + (Sf-Se)
  }
  return(Pg)

}

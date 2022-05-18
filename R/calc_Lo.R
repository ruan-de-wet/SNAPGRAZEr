#' @title Loss of biomass during the off season
#'
#' @description Estimating the amount of biomass lost during the dormant season (dry, non-growing season).
#' @param Cg Daily consumption rate (g/animal/day). This is the average rate during the growing season - for the dormant season, the equations assume half this consumption rate.
#' @param Gdays Total number of days in the growing season. Default = 153 (October to March-ish).
#' @param d Stocking density (head/ha)
#' @export

calc_Lo= function(Cg, Gdays = 153, d) {

  Lo = (Cg/2)*(365-Gdays)*d*10^(-4)
  return(Lo)

}

#' @title Microbial respiration rate in year t
#'
#' @description Calculation of the microbial respiration rate in year t (gC/m2/year), from Ritchie (2020).
#' @param RAIN MAP for year t (mm/year)
#' @param Gdays Total number of days in the growing season.
#' @param SAND Sand % in top 30 cm soil
#' @param DMRESP Daily microbial respiration (gC/m2/year)
#' @param c1 Coefficient 1 from microbial respiration rate linear equation where lowSOC==FALSE. Default based on re-evaluation of published linear equation.
#' @export

calc_MRESPt = function(RAIN, Gdays, SAND, DMRESP, c1 = 0.000358) {

  WETDAYS = (c1*RAIN-0.025)*Gdays
  MRESPt = WETDAYS*(0.7+0.3*SAND/100)*DMRESP
  return(MRESPt)

}

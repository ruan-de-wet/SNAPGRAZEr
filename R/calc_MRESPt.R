#' @title Microbial respiration rate in year t
#'
#' @description Calculation of the microbial respiration rate in year t (gC/m2/year), from Ritchie (2020).
#' @param RAIN MAP for year t (mm/year)
#' @param Gdays Total number of days in the growing season.
#' @param SAND Sand % in top 30 cm soil
#' @param DMRESP Daily microbial respiration (gC/m2/year)
#' @param c2 Coefficient 2. Multiplied by RAIN in the calculation of WETDAYS.
#' @export

calc_MRESPt = function(RAIN, Gdays, SAND, DMRESP, c2 = 0.00044) {

  WETDAYS = (c2*RAIN-0.025)*Gdays
  MRESPt = WETDAYS*(0.7+0.3*SAND/100)*DMRESP
  return(MRESPt)

}

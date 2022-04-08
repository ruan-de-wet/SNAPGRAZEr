#' @title Plant-Derived SOC in year t
#'
#' @description When it comes to soil organic carbon (SOC), there is plant-derived SOC (PDSOC) and dung-derived SOC (DDSOC). PDSOCt just takes the estimated ANPP and BNPP along with LIGCELL, Fire and information about grazing management
#' @param BNPPest Output of calc_BNPPest()
#' @param Sf Biomass at the end of the growing season. Output of calc_Sf().
#' @param Lo Loss of biomass during the off season. Output of calc_Lo().
#' @param LIGCELL Lignin and cellulose content of livestock feed for year t (%)
#' @param FIRE Average number of fires per year (#/year)
#' @export


calc_PDSOCt = function(BNPPest, Sf, Lo, LIGCELL, FIRE) {

  PDSOCt = 0.45 * ( (LIGCELL*(Sf-Lo/2)*(1-FIRE)) + (LIGCELL+0.05)*BNPPest )
  return(PDSOCt)

}

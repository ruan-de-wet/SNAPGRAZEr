#' @title SNAPGRAZE Wrapper Function
#'
#' @description This is a wrapper function around all the SNAPGRAZE equations to calculate SOCeq based on the below input parameters.
#' @param SAND Sand % in top 30 cm soil
#' @param RAIN Long-term Mean Annual Precipitation (mm/year).
#' @param MAT Long-term Mean Annual Precipitation (degrees Celsius).
#' @param FIRE Average number of fires per year (#/year)
#' @param LIGCELL Lignin and cellulose content of livestock feed for year t (%)
#' @param Sk The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.
#' @param S0 Biomass condition prior to the growing season (at the end of the dry season) that is mostly comprised of carbon stores in rhizomes. Default is 0.1*SK.
#' @param Edays Number of days within the growing season prior to grazing episode
#' @param Ddays Number of days of grazing episode
#' @param Fdays Number of days left in the growing season after the grazing episode. Fdays = Gdays - Edays - Ddays
#' @param Gdays Total number of days in the growing season. If Gdays is not specified, then it is estimated based on MAT and RAIN.
#' @param d Stocking density (head/ha)
#' @param n Number of "pastures" per total area, A.
#' @param W Average animal body size (kg live weight)
#' @param Cg Daily consumption rate (g/animal/day)
#' @param r Relative growth rate of grass biomass, which is variable over the growing season given it's density dependence. Can be calculated as tht intercept of a relationship between the measured relative growth rate and biomass at a given time. Default for tropical grasslands is 0.05 and for temperate grasslands is 0.035.
#' @param APCcorrection Default = FALSE. A correction factor for the influence of annual versus perennial plant growth strategies on belowground production. If correction is applied, then APC = 0.291. This is where forage is dominated by annuals (or shrubs often associated with annuals in drier grasslands). Otherwise, APC = 1.
#' @param lowSOC Default = FALSE. Different regression equation for respiration rate is applied for low and high SOC to avoid a negative respiration rate (which isn't physically possible). Threshold for what qualifies as "low SOC" is 4,600 gC/m^2 (i.e. 46 t/ha). Low SOC regression equation is applicable for higher SOC, but just with slightly lower R-squared.
#' @param DEPTH Default = 30. Depth of soil sampling / estimation (cm). The original SNAP model was developed based on measurements to a depth of 40 cm.
#' @export

SNAPGRAZE = function(SAND, RAIN, MAT, FIRE, LIGCELL,
                     Sk = NA, S0 = 0.1*Sk, Edays, Ddays, Fdays, Gdays = NA,
                     d, n, W, Cg = NA, r = 0.05, APCcorrection = FALSE, lowSOC = FALSE, DEPTH = 30) {

  if(is.na(Gdays)){
    Gdays = 22.99*MAT-0.94*MAT^2+0.073*RAIN
  }

  if(is.na(Sk)){
    Sk = calc_ANPPmax(RAIN, MAT)/0.9
  }

  if(is.na(Cg)) {
    Cg = 2*(5300+770*log(W))
  }

  # Episodic Herbivory Model (EHM)
  Se = calc_SE(Sk, Edays, S0, r)
  Lg = calc_Lg(Ddays, d, n, W, Cg)
  Sg = calc_Sg(Sk, Se, Lg, Ddays, n, d, r, W, Cg)
  Sf = calc_Sf(Sk, Sg, r, Fdays)
  Pg = calc_Pg(Se, Sg, Sf, Sk, S0)
  Lo = calc_Lo(Cg, Gdays, d)

  # dmax = calc_dmax(Sf, Sk, Cg, Gdays)

  # if(dmax > d){
  #   print("Looks like stocking density is greater than what is theoretically sustainable.")
  #   } else {print("Stocking density A-O-K!")}

  # Productivity
  ANPPt_max = calc_ANPPmax(RAIN, MAT, SAND)
  ANPPt_est = calc_ANPPest(Se, Sg, Sf, Sk, S0)
  BNPPt_est = calc_BNPPest(RAIN, MAT, ANPPt_est, Sk, S0, APCcorrection, DEPTH)

  # SOC
  PDSOCt = calc_PDSOCt(BNPPt_est, Sf, Lo, LIGCELL, FIRE)
  DDSOCt = calc_DDSOCt(LIGCELL, Ddays, Cg, n, d, Lo)

  SOCeq = calc_SOCeq(PDSOCt, DDSOCt, SAND, RAIN, Gdays, lowSOC)

  return(SOCeq)

}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SNAPGRAZE.R
\name{SNAPGRAZE}
\alias{SNAPGRAZE}
\title{SNAPGRAZE Wrapper Function}
\usage{
SNAPGRAZE(
  SAND,
  RAIN,
  MAT,
  FIRE,
  LIGCELL,
  Sk = NA,
  S0 = 0.1 * Sk,
  Edays,
  Ddays,
  Fdays,
  Gdays = NA,
  d,
  n,
  W,
  Cg = NA,
  r = 0.05,
  APCcorrection = FALSE,
  lowSOC = FALSE,
  DEPTH = 30
)
}
\arguments{
\item{SAND}{Sand \% in top 30 cm soil}

\item{RAIN}{Long-term Mean Annual Precipitation (mm/year).}

\item{MAT}{Long-term Mean Annual Precipitation (degrees Celsius).}

\item{FIRE}{Average number of fires per year (#/year)}

\item{LIGCELL}{Lignin and cellulose content of livestock feed for year t (\%)}

\item{Sk}{The steady state of biomass in the absence of grazing for a given location. This should ideally be measured directly using grazing exclosures.}

\item{S0}{Biomass condition prior to the growing season (at the end of the dry season) that is mostly comprised of carbon stores in rhizomes. Default is 0.1*SK.}

\item{Edays}{Number of days within the growing season prior to grazing episode}

\item{Ddays}{Number of days of grazing episode}

\item{Fdays}{Number of days left in the growing season after the grazing episode. Fdays = Gdays - Edays - Ddays}

\item{Gdays}{Total number of days in the growing season. Default = 153 (October to March-ish).}

\item{d}{Stocking density (head/ha)}

\item{n}{Number of "pastures" per total area, A.}

\item{W}{Average animal body size (kg live weight)}

\item{Cg}{Daily consumption rate (g/animal/day)}

\item{r}{Relative growth rate of grass biomass, which is variable over the growing season given it's density dependence. Can be calculated as tht intercept of a relationship between the measured relative growth rate and biomass at a given time. Default for tropical grasslands is 0.05 and for temperate grasslands is 0.035.}

\item{APCcorrection}{Default = FALSE. A correction factor for the influence of annual versus perennial plant growth strategies on belowground production. If correction is applied, then APC = 0.291. This is where forage is dominated by annuals (or shrubs often associated with annuals in drier grasslands). Otherwise, APC = 1.}

\item{lowSOC}{Default = FALSE. Different regression equation for respiration rate is applied for low and high SOC to avoid a negative respiration rate (which isn't physically possible). Threshold for what qualifies as "low SOC" is 4,600 gC/m^2 (i.e. 46 t/ha). Low SOC regression equation is applicable for higher SOC, but just with slightly lower R-squared.}

\item{DEPTH}{Default = 30. Depth of soil sampling / estimation (cm). The original SNAP model was developed based on measurements to a depth of 40 cm.}
}
\description{
This is a wrapper function around all the SNAPGRAZE equations to calculate SOCeq based on the below input parameters.
}

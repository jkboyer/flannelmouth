#Functions to convert between US and metric units commonly used in Colorado
#river fisheries research
#Author: Jan Boyer, AGFD

#### distance conversions #####

#Convert river miles (RM 0 = Lees Ferry) to kilometers (rkm 0 = Glen Canyon Dam)
MileToKmCOR <- function(river.mile) {
  round(1.60934*(river.mile + 15.8), 2)
}

#Convert river kilometers (rkm 0 = Glen Canyon Dam) to miles (RM 0 = Lees Ferry)
KmToMileCOR <- function(rkm) {
  round(rkm/1.60934 - 15.8, 2)
}

#Standard Mile to kilometer conversion
MileToKm <- function(mile) {
  round(1.60934*(mile), 2)
}

#standard kilometer to mile conversion
KmToMile <- function(km) {
  round(km/1.60934, 2)
}

#### discharge conversions #####
#cubic feet/second to cubic m/second
#round to 3 significant figures, as that is precision of USGS flow gauges
CfsToCms <- function(cfs) {
  signif(cfs/35.314666721489, 3)
}

CmsToCfs <- function(cms) {
  signif(cms*35.314666721489, 3)
}
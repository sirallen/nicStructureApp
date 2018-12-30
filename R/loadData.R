#' @import data.table
#' @import stringr

cc <- fread('data/entityTypeGrouping.csv')
entity.region <- fread('data/EntitiesByRegion.csv')
entity.ofc <- fread('data/EntitiesByOFC.csv')
link.node.ratio <- fread('data/linkNodeRatio.csv')
assets <- fread('data/Assets.csv')

entity.region[, asOfDate:= as.Date(asOfDate)]
entity.ofc[, asOfDate:= as.Date(asOfDate)]
link.node.ratio[, asOfDate:= as.Date(asOfDate)]
assets[, yearqtr:= as.Date(yearqtr)]
# measure in billions
assets[, BHCK2170:= BHCK2170 / 1e6]

HC10bn <- fread('data/LargeHoldingCompanies.csv',
                select = c('Name', 'RSSDID', 'Location', 'TotalAsset'))
HC10bn <- HC10bn[, .(
  `Institution` = str_trim(Name),
  `RSSD ID` = RSSDID,
  Location = sub(',', ', ', Location),
  `9/30/2018 Total Assets (Thousands)` = TotalAsset)]

load('data/bhcList.RData')
load('data/histories.RData')
load('data/coverage.RData')


#' @import data.table
#' @import stringr

cc <- fread('data/entityTypeGrouping.csv')
graphSummary <- fread('data/GraphSummary.csv')
assets <- fread('data/Assets.csv')

graphSummary[, asOfDate:= as.Date(asOfDate)]
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


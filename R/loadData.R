#' @import data.table

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

HC10bn <- fread('data/HC10bn.csv', select = 2:4)
setnames(HC10bn, 3, paste0(names(HC10bn)[3], ' (Thousands)'))

load('data/bhcList.RData')
load('data/histories.RData')
load('data/coverage.RData')

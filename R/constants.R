#' @importFrom gplots col2hex

LEGEND_KEY <- list(
  'Holding Company'       = 'red',
  'Domestic Bank'         = 'darkorange3',
  'Domestic Nonbank'      = '#3182bd',
  'International Bank'    = 'black',
  'International Nonbank' = 'darkmagenta',
  'Finance Company'       = 'limegreen',
  'Data Processing Servicer' = '#116043',
  'Securities Broker/Dealer' = '#ff7373'
)

LEGEND_KEY <- lapply(LEGEND_KEY, col2hex)

BHC_CATEGORIES <- c(
  `BHC` = 'Bank Holding Company',
  `FHC - Domestic` = 'Financial Holding Company - Domestic',
  `FHC - Foreign` = 'Financial Holding Company - Foreign',
  `SLHC` = 'Savings & Loan Holding Company',
  `IHC` = 'Intermediate Holding Companies',
  `FBO as BHC` = 'Foreign Banking Organization as a BHC'
)

RMD_PLOT_START_DATE <- as.Date('1960-01-01')
RMD_PLOT_END_DATE <- as.Date('2017-01-01')
GLB_ACT_DATE <- as.Date('1999-11-12')
BHC_AMEND_DATE <- as.Date('1970-12-31')
BHC_EVENT_LEVELS <- c('Established', 'Changed_to_hc', 'Acquired', 'Closed', 'Changed_from_hc')
CHURN_PLOT_COLORS <- c('#a1d99b','#31a354','#fcbba1','#fb6a4a','#a50f15')
TOTAL_FRY9C_ASSETS_BN_123116 <- 19443

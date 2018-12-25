#' @import data.table
#' @import ggplot2
#' @importFrom gridExtra grid.arrange

get_ymax <- function(ggplot_object) {
  # return y-value of highest minor gridline
  # tail(ggplot_build(ggplot_object)$layout$panel_ranges[[1]]$y.minor_source, 1)
  tail(ggplot_build(ggplot_object)$layout$panel_scales_y[[1]]$range$range, 1)
}

zero_pad_plot1 <- function(dat) {
  # Expand dat to include N = 0 for missing regions for each asOfDate
  # (will fill in unwanted gaps in geom_area due to interpolation)
  dat.0 <- CJ(Id_Rssd  = dat[, Id_Rssd[1]],
             asOfDate = dat[, unique(asOfDate)],
             Region   = dat[, unique(Region)])
  
  dat.0[dat, on = .(Id_Rssd, Region, asOfDate), N:= N]
  dat.0[is.na(N), N:= 0]
  dat.0
}

null_pad_plot3 <- function(dat) {
  # input is a data.table with country counts and labels and "unit"
  # pad with 'NULL' labels (N = 0) if number of states or countries < 10
  n.states.missing <- dat[unit == 'States', 10 - .N]
  n.countries.missing <- dat[unit == 'Countries', 10 - .N]
  
  rbind(
    dat,
    if (n.states.missing > 0) data.table(
      label = paste0(mapply(strrep, ' ', 0:(n.states.missing - 1)), 'NULL'),
      N = 0,
      unit = rep('States', n.states.missing)),
    if (n.countries.missing > 0) data.table(
      label = paste0(mapply(strrep, ' ', 0:(n.countries.missing - 1)), 'NULL'),
      N = 0,
      unit = rep('Countries', n.countries.missing))
  )
}

plotCoverage <- function(spans, start_date = as.Date('2000-03-31')) {
  
  ggplot(spans, aes(x = Name, y = seq.Date(min(start), max(end),
                                       along.with = spans$start))) +
    geom_segment(aes(x = Name, xend = Name,
                     y = pmax(start, start_date),
                     yend = pmax(end, start_date))) +
    geom_point(aes(x = Name, y = pmax(start, start_date)), color = 'red', size = 2) +
    geom_point(aes(x = Name, y = pmax(end, start_date)), color = 'red', size = 2) +
    #scale_y_date(sec.axis = dup_axis()) + # can't do this
    scale_y_date(position = 'top') +
    coord_flip() +
    labs(x = '', y = '')
}

plotEntityCountByRegion <- function(rssd) {
  dat <- entity.region[Id_Rssd == rssd]
  dat.ofc <- entity.ofc[Id_Rssd == rssd]
  lev <- dat[, N[.N], by = 'Region'][order(V1), Region]
  dat[, Region:= factor(Region, lev)]
  dat <- zero_pad_plot1(dat)
  
  # Define "groups" to plot discontinuous geom_areas separately
  # (i.e., breaks when a firm was not an HC)
  dat[, group:= cumsum(c(TRUE, diff(asOfDate) > 92))]
  dat.ofc[, group:= cumsum(c(TRUE, diff(asOfDate) > 92))]
  
  p <- ggplot(dat, aes(x = asOfDate, y = N))
  
  for (g in dat[, unique(group)]) {
    p <- p + geom_area(data = dat[group == g], aes(fill = Region),
                       col = 'lightgray', pos = 'stack', size = .2, alpha = .9) +
      geom_line(data = dat.ofc[group == g],
                aes(x = asOfDate, y = N, color = factor('OFC', labels = str_wrap(
                  'Offshore Financial Centers (IMF Classification)', 30))),
                lwd = 1.3, lty = 2)
  }
  
  p + scale_color_manual(values = 'black') +
    scale_x_date(date_breaks = '2 years', labels = year) +
    labs(x = '', y = 'Number of entities', color = '') +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(order = 2))
}

plotLinkDistanceDistribution <- function(dat, rssd) {
  dat[, linkDist:= min(Tier) - 1, by = 'Id_Rssd']
  dat <- dat[!duplicated(Id_Rssd)][order(linkDist)][, .N, by = 'linkDist']
  dat[, cumShare:= cumsum(N) / sum(N)]
  
  p <- ggplot(dat, aes(x = as.factor(linkDist), y = N)) +
    geom_bar(stat = 'identity', fill = 'royalblue') +
    labs(x = 'Distance from center', y = 'Number of entities')
  
  p + geom_line(aes(x = linkDist, y = cumShare * get_ymax(p)),
                lty = 2, lwd = 1.3, col = 'red') +
    scale_y_continuous(sec.axis = sec_axis(
      ~ . / get_ymax(p), 'Cum. fraction of entities', breaks = seq(0, 1, .25)))
}

plotTop10StatesCountries <- function(dat, rssd) {
  dat <- dat[!duplicated(Id_Rssd)]
  dat[, label:= gsub('.*, *(.*)', '\\1', label)]
  
  dat <- dat[, .N, by = 'label']
  dat[, unit:= ifelse(label %in% c(state.abb, 'DC'), 'States', 'Countries')]
  # full names for states
  dat[unit == 'States', label:= c(state.name, 'District of Columbia')[
    match(label, c(state.abb, 'DC'))]]
  # pad with 'NULL' labels (N = 0) if number of states or countries < 10
  dat <- null_pad_plot3(dat)
  
  dat <- dat[order(unit, N)]
  dat <- dat[dat[, tail(.I, 10), by = 'unit']$V1]
  
  p1 <- ggplot(dat[unit == 'States'], aes(x = factor(label, label), y = N)) +
    geom_bar(stat = 'identity', fill = 'coral') +
    coord_flip() +
    labs(x = '', y = 'Number of entities') +
    ggtitle('Top 10 States')
  
  p2 <- ggplot(dat[unit == 'Countries'], aes(x = factor(label, label), y = N)) +
    geom_bar(stat = 'identity', fill = 'coral') +
    coord_flip() +
    labs(x = '', y = 'Number of entities') +
    ggtitle('Top 10 Countries/Territories (outside U.S.)')
  
  grid.arrange(p1, p2, ncol = 2)
}

plotLinkNodeRatioTs <- function(rssd) {
  dat <- link.node.ratio[Id_Rssd == rssd]
  dat[, discontinuity:= c(FALSE, diff(asOfDate) > 92)]
  dat[, group:= cumsum(discontinuity)]
  
  p <- ggplot(dat, aes(x = asOfDate, y = link.node.ratio)) +
    geom_line(aes(group = group)) +
    scale_x_date(date_breaks = '2 years', labels = year) +
    scale_y_continuous(limits = c(1, NA)) +
    labs(x = '', y = '#Connections / #Subsidiaries')
  
  # dotted lines for discontinuities
  for (d in dat[, which(discontinuity)]) {
    p <- p + geom_path(data = dat[c(d - 1, d)], lty = 3)
  }
  
  p
}

plotEntityAssetConnectedScatter <- function(rssd) {
  dat <- entity.region[Id_Rssd == rssd, .(N = sum(N)), by = 'asOfDate']
  dat.asset <- assets[Id_Rssd == rssd]
  
  dat <- dat[dat.asset, on = .(asOfDate == yearqtr), nomatch = 0]
  dat[, discontinuity:= c(FALSE, diff(asOfDate) > 92)]
  dat[, group:= cumsum(discontinuity)]
  
  p = ggplot(dat, aes(x = BHCK2170, y = N)) +
    geom_point() +
    geom_point(data = dat[month(asOfDate) == 12], col = 'red') +
    geom_path(aes(group = group)) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = 'Assets (billions)', y = 'Number of entities')
  
  # dotted lines for discontinuities
  for (d in dat[, which(discontinuity)]) {
    p <- p + geom_path(data = dat[c(d - 1, d)], lty = 3)
  }
  
  p + geom_text(data = dat[month(asOfDate) == 12],
                aes(label = year(asOfDate) + 1), size = 3, col = 'red',
                nudge_x = -.02 * dat[, max(BHCK2170)])
  
}

plotEntityLinkNodeRatioConnectedScatter <- function(rssd) {
  dat <- entity.region[Id_Rssd == rssd, .(N = sum(N)), by = 'asOfDate']
  dat.ratio <- link.node.ratio[Id_Rssd == rssd]
  
  dat <- dat[dat.ratio, on = 'asOfDate']
  dat[, discontinuity:= c(FALSE, diff(asOfDate) > 92)]
  dat[, group:= cumsum(discontinuity)]
  
  p <- ggplot(dat, aes(x = link.node.ratio, y = N)) +
    geom_point() +
    geom_point(data = dat[month(asOfDate) == 12], col = 'red') +
    geom_path(aes(group = group)) +
    scale_x_continuous(limits = c(1, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = '#Connections / #Subsidiaries', y = 'Number of entities')
  
  # dotted lines for discontinuities
  for (d in dat[, which(discontinuity)]) {
    p <- p + geom_path(data = dat[c(d - 1, d)], lty = 3)
  }
  
  p + geom_text(data = dat[month(asOfDate) == 12],
                aes(label = year(asOfDate) + 1), size = 3, col = 'red',
                nudge_x = -.02 * dat[, max(link.node.ratio) - 1])
  
}


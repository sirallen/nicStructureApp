library(data.table)
library(DT)
library(ggplot2)
library(gplots)
library(gridExtra)
library(knitr)
library(networkD3)
library(rjson)
library(shiny)
library(shinyBS)
library(shinyjs)
library(stringr)

devtools::load_all('.')

COLOR_SCALE <- paste0(
  'd3.scaleOrdinal().domain([', quoteStr(names(LEGEND_KEY)), '])',
  '.range([', quoteStr(LEGEND_KEY), '])')

theme_set(
  theme_bw() +
    theme(panel.border = element_rect(color = NA))
)

bhcMaxTier <- NULL      # updated in observeEvent()
glob.Nodes <- NULL      # updated in renderForceNetwork()
glob.Links <- NULL      # updated in renderForceNetwork()

server <- function(input, output, session) {
  
  observe({
    # Disable options that aren't relevant for active display
    toggleState(selector = 'input[type="checkbox"]', condition = input$dispType == 'Network')
    toggleState(id = 'highlight', condition = input$dispType == 'Network')
    toggleState(id = 'maxDist', condition = input$dispType == 'Map')
  })
  
  observeEvent(input$bhc, {
    new_choices <- rev(gsub(
      '.*-(\\d{4})(\\d{2})(\\d{2}).RData', '\\1-\\2-\\3',
      dir('rdata/', paste0(isolate(input$bhc), '-.*.RData'))))
    
    # If a new bhc is selected and the selected asOfDate is still available,
    # then don't change it; otherwise reset
    x <- intersect(isolate(input$asOfDate), new_choices)
    
    updateSelectInput(
      session, 'asOfDate', choices = new_choices,
      selected = if (length(x) > 0) x else new_choices[1] )
    
    updateSelectInput(session, 'highlight', choices = c('', new_choices))
  })
  
  data <- reactive({
    if (input$bhc != '' && input$asOfDate != '') {
      
      load_data(input$bhc, input$asOfDate)
      
    } else NULL
  })
  
  observeEvent(data(), {
    
    bhcMaxTier <<- data()[[1]][, max(Tier)]
    
  }, priority = 1)

  observeEvent(data(), {
    bhcMaxDist <- bhcMaxTier - 1
    updateSelectInput(session, 'maxDist', choices = 1:bhcMaxDist, selected = bhcMaxDist)
    updateSelectInput(session, 'highlight', selected = '')
  })

  json_data <- reactive({
    if (!is.null(data())) {
      # Important!! Need to use copy(); otherwise will also modify
      # Tier in data()[[3]] -- see http://stackoverflow.com/questions/10225098
      nodes <- copy(data()[[3]])
      nodes[, Tier:= min(Tier), by = 'label']
      nodes <- unique(nodes[, .(Tier, lat, lng, label)])
      nodes <- unname(split(nodes, 1:nrow(nodes)))

      links <- data()[[1]]
      links <- unique(links[, .(Tier, from.lat, from.lng, to.lat, to.lng)])
      links <- unname(split(links, 1:nrow(links)))

      fromJSON(toJSON(list(nodes, links)))

    } else NULL
  })
  
  compare_data <- reactive({
    if (input$highlight != '') {
      # only need nodes (for now)
      load_data(input$bhc, input$highlight)[[2]]
      
    } else NA
  })

  output$network <- renderForceNetwork({
    if (!is.null(data())) {
      links <- copy(data()[[1]])
      nodes <- copy(data()[[2]])
      nodes[, Nodesize:= .1]
      
      if (input$domOnly) {
        # Remove foreign nodes and any descendants which become disconnected
        # from the main graph (may include nodes which are not foreign)
        links[nodes, on = .(from.id == id), from.Group:= i.Group]
        links <- links[!(grepl('International', to.Group) |
                          grepl('International', from.Group))]
        
        # Prune the disconnected pieces
        while (links[, any(!from.id %in% c(0, links$to.id))]) {
          links <- links[from.id %in% c(0, to.id)]
        }
        
        nodes <- nodes[c(TRUE, id[-1] %in% links$to.id)]
        
        updateIds(nodes, links)
      }
      
      if (input$bundle) {
        # Set of links to "terminal" nodes (nodes with 0 children),
        # excluding holding companies
        links.toTerminal <- links[!links[, .(from.id)], on = .(to.id == from.id)][
          !grepl('Holding', to.Group)]
        # Counts of non-HC total children (N) & terminal children (M) for
        # each from.id with M > 0.
        numChildren <- links[!grepl('Holding', to.Group), .N, by = 'from.id'][
          links.toTerminal[, .(M = .N), by = 'from.id'],
          on = 'from.id']
        
        # Choose the 'from' nodes whose children should be bundled
        bundleNodes <- numChildren[N > 3, .(from.id, M)]
        
        # Remove links to (non-HC) terminal children of each node in
        # bundleNodes. Use nomatch=0 in case all terminal children are HCs
        links <- links[!links.toTerminal[bundleNodes, on = 'from.id', nomatch = 0],
                      on = .(from.id, to.id)]
        # Remove those terminal children from 'nodes', update Nodesize
        nodes <- nodes[id %in% c(0, unique(links$to.id))][
          bundleNodes, on = .(id == from.id), Nodesize:= as.double(M + 4)]
        
        updateIds(nodes, links)
      }
      
      # Update globals to keep track of what is plotted (Careful with this --
      # see https://stackoverflow.com/questions/2628621)
      glob.Nodes <<- nodes
      glob.Links <<- links
      
      # USE THE FORCE
      forceNetwork(
        Links = links, Nodes = nodes, Source = 'from.id', Target = 'to.id',
        NodeID = 'name', Group = 'Group', Value = 'value', Nodesize = 'Nodesize',
        zoom = TRUE, opacity = .8,
        opacityNoHover = .5, fontSize = 10, fontFamily = 'sans-serif', arrows = TRUE,
        linkDistance = JS('function(d){return 50}'),
        legend = TRUE, colourScale = JS(COLOR_SCALE)
      )

    } })
  
  output$entityRegionAreaPlot <- renderPlot({
    # Area plot -- number of entities by region
    if (entity.region[Id_Rssd == input$bhc, uniqueN(asOfDate) > 2]) {
      plotEntityCountByRegion(input$bhc)
    }
  })
  
  output$linkDistanceDistributionPlot <- renderPlot({
    # Distribution of entities by link distance from HC
    if (!is.null(data())) {
      plotLinkDistanceDistribution(data()[[3]][-1], input$bhc)
    }
  })
  
  output$top10StatesCountriesPlot <- renderPlot({
    # Most common states / countries
    if (!is.null(data())) {
      plotTop10StatesCountries(data()[[3]], input$bhc)
    }
  })
  
  output$linkNodeRatioTsPlot <- renderPlot({
    # Simple time series: link-node ratio
    if (link.node.ratio[Id_Rssd == input$bhc, .N > 2]) {
      plotLinkNodeRatioTs(input$bhc)
    }
  })
  
  output$entityAssetConnectedPlot <- renderPlot({
    # Connected scatterplot: (n_entities, assets)
    if (assets[Id_Rssd == input$bhc, .N > 2]) {
      plotEntityAssetConnectedScatter(input$bhc)
    }
  })
  
  output$entityLinkNodeRatioConnectedPlot <- renderPlot({
    # Connected scatterplot: (n_entitites, link-node ratio)
    if (link.node.ratio[Id_Rssd == input$bhc, .N] > 2) {
      plotEntityLinkNodeRatioConnectedScatter(input$bhc)
    }
  })
  
  # Make sure to use renderDataTable() from /DT/, not /shiny/
  output$bhcTable <- DT::renderDataTable({
    DT::datatable(
      data()[[1]][, .(Entity = to, Parent = from, Location = to.Loc, Type)]
    )
  })
  
  output$historyTable <- DT::renderDataTable({
    DT::datatable(
      histories[Id_Rssd == as.integer(input$bhc), -1],
      options = list(dom = 't', paging = FALSE, ordering = FALSE)
    )
  })
  
  output$HC10bnTable <- DT::renderDataTable({
    DT::datatable(
      HC10bn, options = list(dom = 't', paging = FALSE, ordering = FALSE)
    )
  })
  
  output$coveragePlot <- renderPlot({
    plotCoverage(spans)
  })
  
  
  ### Observers send messages to _bhcMap.js
  observe({session$sendCustomMessage('toggleLegend', list(input$legend))})
  
  observe({session$sendCustomMessage('jsondata', json_data())})

  observe({session$sendCustomMessage('windowResize', list(input$dimension))})

  observe({session$sendCustomMessage('maxDist', if (input$maxDist != '') {
    list(input$maxDist) } else NULL )})
  
  observeEvent(compare_data(), {
    if (is.data.table(compare_data())) {
      # reset
      glob.Links[, highlight:= FALSE]
      # Add/modify column to identify which nodes/links to highlight. Assuming
      # that the order of the node/link elements in the DOM is the same as the
      # ids. (What about new links between existing nodes? ignoring these
      # for now)
      glob.Nodes[, highlight:= !c(TRUE, Id_Rssd[-1] %in% compare_data()$Id_Rssd)]
      glob.Links[glob.Nodes[highlight == TRUE], on = 'Id_Rssd', highlight:= TRUE]
      glob.Links[glob.Nodes[highlight == TRUE], on = .(Parent == Id_Rssd), highlight:= TRUE]
      glob.Links[, id:= .I - 1L]
      
      session$sendCustomMessage('toggleHighlight',
                                list(input$highlight,
                                     glob.Nodes[highlight == TRUE, id],
                                     glob.Links[highlight == TRUE, id]))
    } else {
      
      session$sendCustomMessage('toggleHighlight', list(FALSE))
    }
  })
  
}


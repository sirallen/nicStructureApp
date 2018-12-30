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

ui <- navbarPage(
  title = 'Visualizing the Structure of U.S. Bank 
  Holding Companies',
  
  tabPanel(
    useShinyjs(),
    
    title = 'Explore',
    
    tags$head(
      tags$link(rel = 'shortcut icon', href = ''),
      includeScript('www/ne_50m_admin.json'),
      includeCSS('www/app.css'),
      includeCSS('www/_bhcMap.css'),
      includeScript('www/_bhcMap.js'),
      includeScript('www/_toggleLegend.js'),
      includeScript('www/_toggleHighlight.js'),
      
      # Can't use d3.select().style() to update legend opacity since drawing
      # new network will override it; need to create a style block, update text
      # directly
      tags$style(
        id = 'legend.style', type = 'text/css',
        'g.legend {opacity: 1; pointer-events: none;}'
      ),
      
      # size of <svg> canvas controlled inside _bhcMap.js
      tags$script(
        'var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension)});
        $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension)});'
      )
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        # "sticky" sidebar
        style = 'position:fixed; width:23%;',
        selectInput(inputId = 'bhc', label = 'Select holding company:',
                    choices = bhcList),
        
        selectInput(inputId = 'asOfDate', label = 'Date:', choices = '2018-09-30'),
        
        radioButtons(inputId = 'dispType', label = 'Select display type:',
                     choices = c('Network','Map')),
        
        checkboxInput(inputId = 'legend', label = 'Show legend', value = TRUE),
        checkboxInput(inputId = 'domOnly', label = 'Hide international entities',
                      value = FALSE),
        checkboxInput(inputId = 'bundle', label = 'Bundle nodes (speeds up rendering 
                      for some large structures)', value = FALSE),
        
        selectInput(inputId = 'highlight', label = 'Compare with:', choices = ''),
        
        # Note: this isn't working in the deployed app
        bsTooltip(id = 'highlight',
                  # need paste0() here
                  title = paste0('Highlight differences with past or future ',
                                 'date. If past, entities created/acquired. ',
                                 'Otherwise destroyed/sold/etc.'),
                  placement = 'right'),
        
        selectInput(inputId = 'maxDist', label = 'Max node distance (map only)',
                    choices = '4'),
        
        width = 3),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = 'Display',
            conditionalPanel(
              "input.dispType == 'Network'",
              forceNetworkOutput('network')),
            
            conditionalPanel(
              "input.dispType == 'Map'",
              div(id = 'd3io', class = 'd3io')
            )
          ),
          
          tabPanel(
            title = 'Table',
            HTML('<br>'),
            DT::dataTableOutput(outputId = 'bhcTable'),
            style = 'font-size:85%'
          ),
          
          tabPanel(
            title = 'Plots',
            HTML('<br>'),
            
            'The following plots are updated in response to changes in 
            user-selected input. (May take several seconds to render.) 
            Not all holding companies have enough data with which to 
            generate plots, so for some selections the plot areas may 
            appear blank.',
            hr(),
            
            h4('Entity count by geographic region'),
            plotOutput('entityRegionAreaPlot', width = '100%', height = '400px'),
            
            h4('Assets vs. entity count'),
            plotOutput('entityAssetConnectedPlot', width = '100%', height = '400px'),
            
            h4('Distribution of entities by edge distance from top-tier HC'),
            plotOutput('linkDistanceDistributionPlot', width = '100%', height = '400px'),
            
            h4('Most-represented states and countries (physical entity location)'),
            plotOutput('top10StatesCountriesPlot', width = '100%', height = '400px'),
            br(),
            
            'Plots below use the ratio of links (connections) to nodes minus one
            (subsidiaries) as a measure of complexity. If each subsidiary has
            exactly one direct parent, the structure is minimally complex with a
            link-node ratio equal to one:',
            
            h4('Link-node ratio'),
            plotOutput('linkNodeRatioTsPlot', width = '100%', height = '400px'),
            
            h4('Link-node ratio vs. entity count'),
            plotOutput('entityLinkNodeRatioConnectedPlot', width = '100%', height = '400px'),
            br()
          ),
          
          tabPanel(
            title = 'History',
            HTML('<br>'),
            
            DT::dataTableOutput(outputId = 'historyTable'),
            style = 'font-size:85%'
          )
          
        ),
        
        width = 9 )
      
    )),
  
  tabPanel(
    title = 'HCs > $10B',
    fluidRow(
      column(3, 'Holding Companies with Assets Greater than $10 Billion'),
      
      column(9, DT::dataTableOutput(outputId = 'HC10bnTable'),
             style = 'font-size:85%')
    )),
  
  tabPanel(
    title = 'Coverage',
    fluidRow(
      column(3, 'This chart shows the time period(s) for which structure
             data is available for each holding company. Discontinuous
             segments indicate that an institution may have changed its status
             to something other than a holding company.',
             
             HTML('<br><br>'),
             
             'Note that each row really traces a particular RSSD; the name
             displayed is the one most recently associated with the RSSD, which
             is not shown.'),
      
      column(9, plotOutput(outputId = 'coveragePlot', height = '1200px'))
      )),
  
  tabPanel(
    title = 'Background',
    fluidRow(
      column(2),
      column(8, includeMarkdown('Rmarkdown/Background.md')),
      column(2)
    )
  ),
  
  tabPanel(
    title = 'About',
    fluidRow(
      column(2),
      column(8, includeMarkdown('Rmarkdown/About.md')),
      column(2)
    )),
  
  # Additional navbarPage() options
  fluid = TRUE, inverse = TRUE, position = 'fixed-top',
  windowTitle = 'shinyApp'
)


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
    
    bhcMaxTier <<- data()$links[, max(Tier)]
    
  }, priority = 1)

  observeEvent(data(), {
    bhcMaxDist <- bhcMaxTier - 1
    updateSelectInput(session, 'maxDist', choices = 1:bhcMaxDist, selected = bhcMaxDist)
    updateSelectInput(session, 'highlight', selected = '')
  })

  json_data <- reactive({
    if (!is.null(data())) {
      # Important!! Need to use copy(); otherwise will also modify
      # Tier in data()$df -- see http://stackoverflow.com/questions/10225098
      nodes <- copy(data()$df)
      nodes[, Tier:= min(Tier), by = 'label']
      nodes <- unique(nodes[, .(Tier, lat, lng, label)])
      nodes <- unname(split(nodes, 1:nrow(nodes)))

      links <- data()$links
      links <- unique(links[, .(Tier, from.lat, from.lng, to.lat, to.lng)])
      links <- unname(split(links, 1:nrow(links)))

      fromJSON(toJSON(list(nodes, links)))

    } else NULL
  })
  
  compare_data <- reactive({
    if (input$highlight != '') {
      # only need nodes (for now)
      load_data(input$bhc, input$highlight)$nodes
      
    } else NA
  })

  output$network <- renderForceNetwork({
    if (!is.null(data())) {
      links <- copy(data()$links)
      nodes <- copy(data()$nodes)
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
    if (entity.region[Id_Rssd == input$bhc, uniqueN(asOfDate) > 2]) {
      plotEntityCountByRegion(input$bhc)
    }
  })
  
  output$linkDistanceDistributionPlot <- renderPlot({
    if (!is.null(data())) {
      plotLinkDistanceDistribution(data()$df[-1], input$bhc)
    }
  })
  
  output$top10StatesCountriesPlot <- renderPlot({
    if (!is.null(data())) {
      plotTop10StatesCountries(data()$df, input$bhc)
    }
  })
  
  output$linkNodeRatioTsPlot <- renderPlot({
    if (link.node.ratio[Id_Rssd == input$bhc, .N > 2]) {
      plotLinkNodeRatioTs(input$bhc)
    }
  })
  
  output$entityAssetConnectedPlot <- renderPlot({
    if (assets[Id_Rssd == input$bhc, .N > 2]) {
      plotEntityAssetConnectedScatter(input$bhc)
    }
  })
  
  output$entityLinkNodeRatioConnectedPlot <- renderPlot({
    if (link.node.ratio[Id_Rssd == input$bhc, .N] > 2) {
      plotEntityLinkNodeRatioConnectedScatter(input$bhc)
    }
  })
  
  output$bhcTable <- DT::renderDataTable({
    DT::datatable(
      data()$links[, .(Entity = to, Parent = from, Location = to.Loc, Type)]
    )
  })
  
  output$historyTable <- DT::renderDataTable({
    DT::datatable(
      histories[Id_Rssd == as.integer(input$bhc), -1],
      options = list(dom = 't', paging = FALSE, ordering = FALSE)
    )
  })
  
  output$HC10bnTable <- DT::renderDataTable({
    HC10bn %>%
      mutate_at('9/30/2018 Total Assets (Thousands)', scales::dollar) %>%
      DT::datatable(
        options = list(dom = 't', paging = FALSE, ordering = FALSE)
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

shinyApp(ui = ui, server = server)

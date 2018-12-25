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
            
            plotOutput('entityRegionAreaPlot', width = '80%', height = '600px'),
            
            'Connected scatterplot to track growth along two dimensions:',
            
            plotOutput('entityAssetConnectedPlot', width = '80%', height = '600px'),
            plotOutput('linkDistanceDistributionPlot', width = '80%', height = '600px'),
            plotOutput('top10StatesCountriesPlot', width = '80%', height = '600px'),
            
            'Plots below use the ratio of links (connections) to nodes minus one
            (subsidiaries) as a measure of complexity. If each subsidiary has
            exactly one direct parent, the structure is minimally complex with a
            link-node ratio equal to one:',
            
            plotOutput('linkNodeRatioTsPlot', width = '80%', height = '600px'),
            plotOutput('entityLinkNodeRatioConnectedPlot', width = '80%', height = '600px')),
          
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


#! /usr/bin/Rscript
# ---------- Read in arguments, setup options ----------
require('docopt', quietly = TRUE)
"Run Shiny app for SwingPlanIt events. 
Use a dbname ending with .rda.

Usage:
  app.R  [<dbname>]

Options:
-h --help         Show this" -> doc

args <- docopt::docopt(doc) 

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(lubridate)

# This stuff won't change for each user, load it as little as possible
if (is.null(args$dbname)){
    load(file = 'data/swp_db.rda')
} else {
    load(file = args$dbname)
}
    
dance_styles <- list('Balboa', 'Boogie Woogie', 'Blues', 'Charleston', 'Jazz', 'Lindy Hop', 'Shag')
continents <- list('Africa', 'Australasia', 'Asia', 'Europe', 'North America', 'South America')
countries <- as.list(
    event_table %>%
        select(country) %>%
        unique() %>%
        do.call(paste, .) %>%
        sort()
)


#' @param text Character
#' @param search Character. Will be passed to stringr::regex, with ignore_case = TRUE
#' @return Highlighted HTML
highlight_search <- function(text, search){
    replacer <- function(x){
        as.character(span(class = 'highlight-search', x))
    }
    if (search != ''){
        tryCatch({
            HTML(stringr::str_replace_all(text,
                                          stringr::regex(search, ignore_case = TRUE),
                                          replacer))
        }, error = function(e) text)
    } else {
        return (HTML(text))
    }
}


ui <- bootstrapPage(
    navbarPage(
        "Swing Plan-It Explorer",
        id = 'nav',
        inverse = TRUE,       
        
        # Map panel
        tabPanel(
            'Map View',
            
            # Include CSS
            div(class="outer",
                tags$head(
                    includeCSS("www/styles.css"),
                ),
                
                # Map
                leafletOutput("mymap",
                              width = "100%",
                              height = "100%"),
                
                # Control panel
                absolutePanel(
                    id = 'controls',
                    class = "panel panel-default",
                    fixed = TRUE,
                    draggable = FALSE,
                    top = 130,
                    right = "auto",
                    left = 10,
                    bottom = "auto",
                    width = 280,
                    height = "auto",
                    
                    h4(class = 'centre-title', strong('Filters')),
                    
                    # Filters
                    inputPanel(
                        dateRangeInput('date_range',
                                       'Select date range',
                                       end = lubridate::now() + lubridate::years(1)),
                        
                        selectInput('continent_filter',
                                    'Filter by continent', 
                                    continents,
                                    multiple = TRUE),
                        
                        selectInput('country_filter',
                                    'Filter by country', 
                                    countries,
                                    multiple = TRUE),
                        
                        selectInput('style_search',
                                    label = 'Select dance styles',
                                    choices = dance_styles,
                                    multiple = TRUE),
                        
                        textInput('teacher_search',
                                  label = 'Search teacher description',
                                  placeholder = 'Joe le Taxi'),
                        
                        textInput('description_search',
                                  label = 'Search event description',
                                  placeholder = 'Hot Sugar Band')
                    )
                ),

                # Summary panel
                absolutePanel(
                    id = 'summary',
                    class = "panel panel-default",
                    fixed = TRUE,
                    draggable = TRUE,
                    top = 70,
                    left = "auto",
                    right = 10,
                    bottom = "auto",
                    width = 320,
                    height = "auto",
                    
                    uiOutput('event_summary')
                )
            )
        ),
        
        tabPanel(
            'About')
    )
)


server <- function(input, output) {

    # Base structures
    base_map_data <- event_table
    
    # Triggers
    filtering_triggers <- reactive(c(input$date_range,
                                     input$teacher_search,
                                     input$description_search,
                                     input$style_search,
                                     input$continent_filter,
                                     input$country_filter))

    event_summary_triggers <- reactive(c(input$mymap_marker_click,
                                         input$mymap_marker_mouseover))
    
    # Update data to display
    filtered_data <- eventReactive(filtering_triggers(), {
        o <- base_map_data %>%
            filter(start_date > input$date_range[1] & end_date < input$date_range[2])
        
        if (length(input$style_search) > 0){
            evs <- style_table %>%
                filter(style %in% input$style_search) %>%
                select(event_code) %>%
                unique()
            o <- o %>%
                semi_join(evs, by = c('event_code' = 'event_code'))
        }
        if (length(input$continent_filter) > 0){
            o <- o %>%
                filter(continent %in% input$continent_filter)
        }
        if (length(input$country_filter) > 0){
            o <- o %>%
                filter(country %in% input$country_filter)
        }
        if (length(input$teacher_search) > 0){
            o <- o %>%
                filter(tryCatch(
                    {grepl(input$teacher_search, teacher_description, ignore.case = TRUE)},
                    error = function(e) FALSE)
                )
        }
        if (length(input$description_search) > 0){
            o <- o %>%
                filter(tryCatch(
                    {grepl(input$description_search, description, ignore.case = TRUE)},
                    error = function(e) FALSE)
                )
        }
        
        return (o)
    })

    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            setView(-5, 10, 2)
    })

    observeEvent(filtering_triggers(), {
        leafletProxy('mymap') %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            addMarkers(lng = filtered_data()$longitude,
                       lat = filtered_data()$latitude,
                       layerId = filtered_data()$event_code,
                       clusterOptions = markerClusterOptions())
    })
    
    
    # Update event description content
    event_info <- eventReactive(event_summary_triggers(), {
        layer_id <- ifelse(is.null(input$mymap_marker_mouseover$id),
                           input$mymap_marker_click$id,
                           input$mymap_marker_mouseover$id)
                           
        if(! is.null(layer_id)){
            l <- filtered_data() %>%
                filter(event_code == layer_id) %>%
                mutate(start_date = as.character(start_date),
                       end_date = as.character(end_date)) %>%
                as.list()
            l <- lapply(l, function(x) ifelse(is.null(x) || is.na(x), 'Unknown', as.character(x)))
            l$dance_styles <- style_table %>%
                filter(event_code == layer_id) %>%
                select(style) %>%
                do.call(paste, .)
            
            return (l)
        }
    })

    # Build event summary HTML
    event_summary <- eventReactive(event_info(), {
        if (length(event_info()) > 0){
            list(h3(class = 'centre-title',
                    event_info()$name),
                 
                 p(event_info()$location,
                   paste0('(', event_info()$country, ')')
                 ),
                 
                 p('From ', strong(event_info()$start_date),
                   ' to ', strong(event_info()$end_date)
                 ),
                 
                 p(strong(event_info()$event_format),
                   strong(' -', paste0(event_info()$dance_styles,
                              collapse = ', '))),
                 
                 hr(),
                 
                 p(highlight_search(event_info()$description,
                                    input$description_search)),
                 
                 hr(),
                 
                 p(highlight_search(event_info()$teacher_description,
                                    input$teacher_search))
            )
        }
    })
    
    # Render event description
    output$event_summary <- renderUI(event_summary())

}

# Run the application 
shinyApp(ui = ui, server = server)

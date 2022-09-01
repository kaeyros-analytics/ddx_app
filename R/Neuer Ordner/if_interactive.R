
if (interactive()) {
  library(shiny)
  library(dslabs)
  library(dplyr)
  #> 
  #> Attaching package: 'dplyr'
  #> The following objects are masked from 'package:stats':
  #> 
  #>     filter, lag
  #> The following objects are masked from 'package:base':
  #> 
  #>     intersect, setdiff, setequal, union
  library(tidyr)  # just to use drop_na()
  library(ggplot2)
  library(plotly)
  #> 
  #> Attaching package: 'plotly'
  #> The following object is masked from 'package:ggplot2':
  #> 
  #>     last_plot
  #> The following object is masked from 'package:stats':
  #> 
  #>     filter
  #> The following object is masked from 'package:graphics':
  #> 
  #>     layout
  library(shinythemes)
  
  data("gapminder")
  
  # Doing preprocessing before any Shiny-specific code
  west <- c("Western Europe","Northern Europe","Southern Europe",
            "Northern America","Australia and New Zealand")
  
  gapminder <- gapminder %>%
    mutate(region_group = case_when(
      region %in% west ~ "The West",
      region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
      region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
      continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
      TRUE ~ "Others")) %>%
    # Simply dropping any NaNs
    drop_na() %>%
    mutate(gpd_per_capita = gdp/population) %>%
    mutate(population_in_millions = population/10^6)
  
  ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("World Health & Economic Data - Gapminder"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "yearInput", label = "Year", 
                    min = min(gapminder$year), max = max(gapminder$year), 
                    value = c(1970, 2011),
                    sep = ""
        ),
        # Using selectizeInput for multiple = TRUE because of verstatile UI
        selectizeInput(inputId = "countryInput", label = "Country",
                       choices = gapminder$country,
                       multiple=TRUE,
                       options = list(
                         'plugins' = list('remove_button'))
        ),
        selectInput("metricInput", "Metric",
                    choices = c("infant_mortality", "life_expectancy", "gpd_per_capita")
        ),
        actionButton("button", "Plot!")
        
      ),
      mainPanel(
        plotlyOutput("coolplot")
      )
    )
  )
  #> Warning: The select input "countryInput" contains a large number of options;
  #> consider using server-side selectize for massively improved performance. See the
  #> Details section of the ?selectizeInput help topic.

  server <- function(input, output, session) {
    # Defining filtered dataframe outside renders
    filtered <- eventReactive(input$button,{
      # To stop errors popping up in app if nothing is chosen by default
      if (is.null(input$countryInput) || is.null(input$yearInput)) {
        return(NULL)
      }
      gapminder %>%
        # Filter based on the interactive input 
        filter(year >= input$yearInput[1],
               year <= input$yearInput[2],
               country %in% input$countryInput
        )
    })
    
    trial <- eventReactive(input$button, {
      input$metricInput
    })
    
    # Create reactive output for coolplot
    observeEvent(input$button, {
      output$coolplot <- renderPlotly({
        # if (is.null(input$countryInput) ||
        #     is.null(input$yearInput)) {
        #   return(NULL)
        # }
        
        p <- ggplot(filtered(), 
                    # aes_string allows us to change the y-label based on reactive metricInput
                    aes_string(x = "year", y = trial(), col = "country", 
                               size = "population_in_millions")) +
          geom_point(alpha = 0.8)
        
        p %>% ggplotly()
      })
    })
    
  }
  
  shinyApp(ui, server)
} # interactive

####

if(interactive()){
  library(leaflet)
  m <- leaflet::leaflet() %>% addTiles() %>% setView(-71.0382679, 42.3489054, zoom = 18)
  m  # the RStudio 'headquarter'
  m %>% fitBounds(-72, 40, -70, 43)
  m %>% clearBounds()  # world view
}



server <- function(input, output, session) {
  # Defining filtered dataframe outside renders
  filtered <- eventReactive(input$button,{
    # To stop errors popping up in app if nothing is chosen by default
    if (is.null(input$countryInput) || is.null(input$yearInput)) {
      return(NULL)
    }
    gapminder %>%
      # Filter based on the interactive input 
      filter(year >= input$yearInput[1],
             year <= input$yearInput[2],
             country %in% input$countryInput
      )
  })
  
  trial <- eventReactive(input$button, {
    input$metricInput
  })
  
  
  
  
  # Create reactive output for coolplot
  observeEvent(input$button, {
    output$coolplot <- renderPlotly({
      # if (is.null(input$countryInput) ||
      #     is.null(input$yearInput)) {
      #   return(NULL)
      # }
      
      p <- ggplot(filtered(), 
                  # aes_string allows us to change the y-label based on reactive metricInput
                  aes_string(x = "year", y = trial(), col = "country", 
                             size = "population_in_millions")) +
        geom_point(alpha = 0.8)
      
      p %>% ggplotly()
    })
  })
  
}

if (interactive()){
  library(shiny)
  library(leaflet)
  
  ui <- shinyUI(navbarPage("Beams", id="nav",
                     
                     tabPanel("Interactive map",
                              div(class="outer",
                                  leafletOutput("map", "100%", 650),
                                  actionButton("drawPoints", "Draw")
                              )
                     )
  ))
  
  library(shiny)
  library(leaflet)
  
  data <- list(
    beam1 = data.frame(lon = c(-115,-125, -125, -115, -115),
                       lat = c(32, 32, 45, 45, 32)),
    beam2 =     data.frame(lon = c(-100, -111, -111, -100, -100),
                           lat = c(42, 42, 50, 50, 42))
  )
  
   server <- shinyServer(function(input, output, session) {
    
    map <- leaflet() %>% addTiles() %>% setView(-93.85, 37.45, zoom = 4)
    output$map <- renderLeaflet(map)
    proxy <- leafletProxy("map", deferUntilFlush = FALSE)
    
    observeEvent(input$drawPoints, {
      proxy %>% clearShapes()
      for (i in seq_along(data)) {
        proxy %>% addPolygons(
          data[[i]][,"lon"],
          data[[i]][,"lat"],
          layerId=i,
          opacity=0.4,
          color = c('red','green')[i]
        )
        Sys.sleep(2)  # - this is to see first (red) polygon
      }
    })
  })
  shinyApp(ui, server)
  
}

# checkbox group: https://riptutorial.com/r/example/12281/checkbox-group

if(interactive()){
  library(shiny)
  
  ui <- fluidPage(    
    checkboxGroupInput("checkGroup1", label = h3("This is a Checkbox group"), 
                       choices = list("1" = 1, "2" = 2, "3" = 3),
                       #choices = c(1,  2, 3),
                       #inline = FALSE,
                       selected = 1),
    fluidRow(column(3, verbatimTextOutput("text_choice")))
  ) 
  
  
  server <- function(input, output){
    output$text_choice <- renderPrint({
      return(paste0("You have chosen the choice ",input$checkGroup1))})
  } 
  
  shinyApp(ui = ui, server = server)
}

# shiny control widgets: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

if(interactive()){
  library(shiny)
  
  # Define UI ----
  ui <- fluidPage(
    titlePanel("Basic widgets"),
    
    fluidRow(
      
      column(3,
             h3("Buttons"),
             actionButton("action", "Action"),
             br(),
             br(), 
             submitButton("Submit")),
      
      column(3,
             h3("Single checkbox"),
             checkboxInput("checkbox", "Choice A", value = TRUE)),
      
      column(3, 
             checkboxGroupInput("checkGroup", 
                                h3("Checkbox group"), 
                                choices = list("Choice 1" = 1, 
                                               "Choice 2" = 2, 
                                               "Choice 3" = 3),
                                selected = 1)),
      
      column(3, 
             dateInput("date", 
                       h3("Date input"), 
                       value = "2014-01-01"))   
    ),
    
    fluidRow(
      
      column(3,
             dateRangeInput("dates", h3("Date range"))),
      
      column(3,
             fileInput("file", h3("File input"))),
      
      column(3, 
             h3("Help text"),
             helpText("Note: help text isn't a true widget,", 
                      "but it provides an easy way to add text to",
                      "accompany other widgets.")),
      
      column(3, 
             numericInput("num", 
                          h3("Numeric input"), 
                          value = 1))   
    ),
    
    fluidRow(
      
      column(3,
             radioButtons("radio", h3("Radio buttons"),
                          choices = list("Choice 1" = 1, "Choice 2" = 2,
                                         "Choice 3" = 3),selected = 1)),
      
      column(3,
             selectInput("select", h3("Select box"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                        "Choice 3" = 3), selected = 1)),
      
      column(3, 
             sliderInput("slider1", h3("Sliders"),
                         min = 0, max = 100, value = 50),
             sliderInput("slider2", "",
                         min = 0, max = 100, value = c(25, 75))
      ),
      
      column(3, 
             textInput("text", h3("Text input"), 
                       value = "Enter text..."))   
    )
    
  )
  
  # Define server logic ----
  server <- function(input, output) {
    
  }
  
  # Run the app ----
  shinyApp(ui = ui, server = server)
}

# drop-down checkbox input in shiny: https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny

# function dropdownButton

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

if(interactive()){
  library("shiny")
  ui <- fluidPage(
    tags$h1("Example dropdown button"),
    br(),
    fluidRow(
      column(
        width = 6,
        dropdownButton(
          label = "Check some boxes", status = "default", width = 80,
          checkboxGroupInput(inputId = "check1", label = "Choose", choices = paste(1:26, ") Choice ", LETTERS))
        ),
        verbatimTextOutput(outputId = "res1")
      ),
      column(
        width = 6,
        dropdownButton(
          label = "Check some boxes", status = "default", width = 80,
          actionButton(inputId = "a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
          actionButton(inputId = "z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
          br(),
          actionButton(inputId = "all", label = "(Un)select all"),
          checkboxGroupInput(inputId = "check2", label = "Choose", choices = paste(1:26, ") Choice ", LETTERS))
        ),
        verbatimTextOutput(outputId = "res2")
      )
    )
  )
  server <- function(input, output, session) {
    output$res1 <- renderPrint({
      input$check1
    })
    
    # Sorting asc
    observeEvent(input$a2z, {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", choices = paste(1:26, ") Choice ", LETTERS), selected = input$check2
      )
    })
    # Sorting desc
    observeEvent(input$z2a, {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", choices = paste(26:1, ") Choice ", rev(LETTERS)), selected = input$check2
      )
    })
    output$res2 <- renderPrint({
      input$check2
    })
    # Select all / Unselect all
    observeEvent(input$all, {
      if (is.null(input$check2)) {
        updateCheckboxGroupInput(
          session = session, inputId = "check2", selected = paste(1:26, ") Choice ", LETTERS)
        )
      } else {
        updateCheckboxGroupInput(
          session = session, inputId = "check2", selected = ""
        )
      }
    })
  }
  shinyApp(ui = ui, server = server)
}

if(interactive()){
  ui <- fluidPage(
    radioButtons("rb", "Choose one:",
                 choiceNames = list(
                   icon("calendar"),
                   HTML("<p style='color:red;'>Red Text</p>"),
                   "Normal text"
                 ),
                 choiceValues = list(
                   "icon", "html", "text"
                 )),
    textOutput("txt")
  )
  
  server <- function(input, output) {
    output$txt <- renderText({
      paste("You chose", input$rb)
    })
  }
  
  shinyApp(ui, server)

}


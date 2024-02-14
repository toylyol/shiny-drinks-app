
#**********************************
# See Chapter 10 of Mastering Shiny ----

library(shiny)

ui <- fluidPage(
  sliderInput("x1", "x1", 0, min = -10, max = 10),
  sliderInput("x2", "x2", 0, min = -10, max = 10),
  sliderInput("x3", "x3", 0, min = -10, max = 10),
  actionButton("reset", "Reset")
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(inputId = "x1", value = 0)
    updateSliderInput(inputId = "x2", value = 0)
    updateSliderInput(inputId = "x3", value = 0)
  })
}

shinyApp(ui, server)


#******************************************
# See Hierarchical Select in Chapter 10.1.2 ----


# Load packages

packages <- c("shiny", "dplyr", "stringr", "magrittr")

invisible(lapply(packages, library, character.only = TRUE))


# Load data

raw_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')


# Clean up data for my purposes

starbucks <- raw_data |>
  mutate(category = case_when(str_detect(product_name, 
                                         "Chai|Tea|tea") ~ "Tea",
                              str_detect(product_name,
                                         "Lemonade|Smoothie|Hot Chocolate|Fibre Powder|Refresher|Apple Spice|Crème Frappuccino Blended") ~ "Other",
                              TRUE ~ "Coffee"
  ),
  .after = product_name
  ) |> 
  mutate(product_name = str_to_title(product_name)) |> 
  mutate(size = str_to_title(size))


# Create the app itself

ui <- fluidPage(
  
  selectInput(inputId = "select_cat", 
              label = "Category", 
              choices = sort(unique(starbucks$category)),
              multiple = TRUE),
  selectInput(inputId = "select_bev",
              label = "Drinks",
              choices = NULL,
              multiple = TRUE),
  selectInput(inputId = "select_size",
              label = "Size",
              choices = NULL,
              multiple = TRUE),
  tableOutput("table")
  
)

server <- function(input, output, session) {
  
  category <- reactive({
    filter(starbucks, category %in% input$select_cat)
  })
  
  observeEvent(category(), {
    choices <- category()$product_name |> 
      unique() |> 
      sort()
    updateSelectInput(inputId = "select_bev", choices = choices) 
  })
  
  drinks <- reactive({
    req(input$select_bev)
    filter(category(), product_name %in% input$select_bev)
  })
  
  observeEvent(drinks(), {
    choices <- drinks()$size |> 
      unique() |> 
      sort()
    updateSelectInput(inputId = "select_size", choices = choices)
  })
  
  output$table <- renderTable({
    req(input$select_size)
    drinks() |>  
      filter(size %in% input$select_size)
  })
  
}

shinyApp(ui, server)


#********************
# Chapter 3 Observers ----

library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting"),
  span(
    "Example",
    div(style = "display:inline-block;",
        title = "A tooltip",
        icon("info-circle")))
)

server <- function(input, output, session) {
  string <- reactive(paste0("Hello ", input$name, "!"))
  
  output$greeting <- renderText(string())
  observeEvent(input$name, {
    message("Greeting performed")  # The message prints to console.
  })
}

shinyApp(ui, server)


#******************************
# Failed attempt to add tabsets ----


# Load packages

packages <- c("shiny", "dplyr", "stringr", "ggplot2", "reactable")

invisible(lapply(packages, library, character.only = TRUE))


# Load data

raw_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv',
                            show_col_types = FALSE)


# Clean up data for my purposes

starbucks <- raw_data |>
  mutate(category = case_when(str_detect(product_name, 
                                         "Chai|Tea|tea") ~ "Tea",
                              str_detect(product_name,
                                         "Lemonade|Smoothie|Hot Chocolate|Fibre Powder|Refresher|Apple Spice|Crème Frappuccino Blended") ~ "Other",
                              TRUE ~ "Coffee"
  ), 
  .before = product_name
  ) |> 
  mutate(product_name = str_to_title(product_name)) |> 
  mutate(size = str_to_title(size))


# Create the app itself

ui <- fluidPage(
  
  titlePanel("Starbucks Nutrition"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("select_cat", 
                  label = "Category", 
                  choices = sort(unique(starbucks$category)),
                  multiple = TRUE),
      
      uiOutput("select_bev"), # name child
      
      uiOutput("select_bev") # name child
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Chart", plotOutput("chart")),
                  tabPanel("Table", reactableOutput("table"))
      )
    )
    
  )
  
)

server <- function(input, output, session) {
  
  output$select_bev <- renderUI({
    drinks <- starbucks %>%
      filter(category %in% input$select_cat) %>%
      pull(product_name) %>%
      unique() %>%
      sort()
    
    selectInput("select_bev", 
                label = "Drink",
                choices = drinks,
                multiple = TRUE
    )
  })
  
  output$select_size <- renderUI({
    sizes <- starbucks %>%
      filter(product_name %in% input$select_bev) %>%
      pull(size) %>%
      unique() %>%
      sort()
    
    selectInput("select_size", "Size",
                choices = sizes,
                multiple = TRUE
    )
  })
  
  dataset <- reactive({
    
    if( !is.null(input$select_cat) & !is.null(input$select_bev) & !is.null(input$select_size) ){
      starbucks |>
        filter(category %in% input$select_cat)|> 
        filter(product_name %in% input$select_bev) |> 
        filter(size %in% input$select_size)  } else 
          if(!is.null(input$select_cat) & !is.null(input$select_bev) & is.null(input$select_size)){
            starbucks |>
              filter(category %in% input$select_cat)|> 
              filter(product_name %in% input$select_bev)} else
                if(!is.null(input$select_cat) & is.null(input$select_bev) & is.null(input$select_size)){
                  starbucks |> filter(category %in% input$select_cat)} else {
                    starbucks
                  }
    
  })
  
  ouput$chart <- renderPlot({
    plot(dataset()$calories, dataset()$sugar_g)
  })
  
  output$table <- renderReactable({
    reactable(dataset())
  })
  
}

shinyApp(ui, server)

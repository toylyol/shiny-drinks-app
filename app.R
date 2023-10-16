
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
    drinks() %>% 
      filter(size %in% input$select_size)
  })
  
}

shinyApp(ui, server)

# See Chapter 10.1.2 at https://mastering-shiny.org/action-dynamic.html#hierarchical-select

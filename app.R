
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
  
  selectInput("select_cat", 
              label = "Category", 
              choices = sort(unique(starbucks$category)),
              multiple = TRUE,
              selectize = TRUE
              ),
  uiOutput("select_bev"), # name child
  uiOutput("select_size") # name grandchild
  
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
  
}

shinyApp(ui, server)
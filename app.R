
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

plot(raw_data$calories, raw_data$sugar_g)



# Create the app itself

ui <- fluidPage(
  
  selectInput("select_cat", 
              label = "Category", 
              choices = sort(unique(starbucks$category)),
              multiple = TRUE,
              selectize = TRUE
  ),
  uiOutput("select_bev"), # name child
  uiOutput("select_size"), # name grandchild
  plotOutput("chart"),
  reactableOutput("table")
  
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
  
  output$chart <- renderPlot({
    plot(dataset()$calories, dataset()$sugar_g)
    })
  
  output$table <- renderReactable({
    reactable(dataset())
  })
  
}

shinyApp(ui, server)

# https://www.linkedin.com/pulse/shiny-app-r-integrating-filter-multiple-dynamic-conditions-lee-rock/
# https://community.rstudio.com/t/reactive-filter-using-multiple-inputs-in-shiny/28974
# https://www.davidsolito.com/post/conditional-drop-down-in-shiny/

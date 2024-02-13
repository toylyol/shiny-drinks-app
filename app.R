
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
                              TRUE ~ "Coffee" ), .before = product_name
         ) |> 
  mutate(product_name = str_to_title(product_name)) |> 
  mutate(size = str_to_title(size)) |>
  mutate(whip = case_when(whip == 0 ~ "No", 
                          whip == 1 ~ "Yes"),
         milk = case_when(milk == 0 ~ "None",
                          milk == 1 ~ "Nonfat",
                          milk == 2 ~ "2%",
                          milk == 3 ~ "Soy",
                          milk == 4 ~ "Coconut",
                          milk == 5 ~ "Whole")
         ) |> 
  select (-c(serv_size_m_l, saturated_fat_g, trans_fat_g, sodium_mg, total_carbs_g, fiber_g))
  

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
    
    ggplot() +
      geom_point(data = dataset(), 
                 aes(x = calories, y = sugar_g, 
                     size = caffeine_mg, color = category),
                 alpha = 0.5) +
      scale_size(range = c(0, 10)) + 
      theme_minimal()
    
    })
  
  output$table <- renderReactable({
    
    dataset() |>
      rename (Category = category, 
              Product = product_name,
              Size = size,
              Calories = calories,
              `Milk Type` = milk,
              `Whipped Topping` = whip,
              `Total Fat (g)` = total_fat_g,
              `Cholesterol (mg)` = cholesterol_mg,
              `Sugar (g)` = sugar_g,
              `Caffeine (mg)` = caffeine_mg) |>
      reactable()
    
  })
  
}

shinyApp(ui, server)

# TODO:
# Choose different variable to scale radius.
# Choose color scheme. 
# Choose different variable to tie to color scheme.
# Fix coordinate system and legends.
# Convert using ggplotly().


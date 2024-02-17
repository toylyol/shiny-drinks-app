
# Load packages

packages <- c("shiny", "dplyr", "stringr", "ggplot2", "reactable", "bslib")

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
  select (-c(serv_size_m_l, saturated_fat_g, trans_fat_g, sodium_mg, total_carbs_g, fiber_g)) |>
  rename (Category = category, 
          Drink = product_name,
          Size = size,
          Calories = calories,
          `Milk Type` = milk,
          `Whipped Topping` = whip,
          `Total Fat (g)` = total_fat_g,
          `Cholesterol (mg)` = cholesterol_mg,
          `Sugar (g)` = sugar_g,
          `Caffeine (mg)` = caffeine_mg)


# Create the app itself

ui <- page_sidebar(
  
  theme = bs_theme(preset = "minty",
                   secondary = "#54b198"),
  
  title = "Shiny Drinks",
  
  sidebar = sidebar(
    
    selectInput("select_cat", 
                label = "Category", 
                choices = sort(unique(starbucks$Category)),
                multiple = TRUE,
                selectize = TRUE), # create parent
    uiOutput("select_bev"), # name child
    uiOutput("select_size"), # name grandchild
    actionButton("reset_button", "Reset Filters")
  ), # end sidebar
  
  navset_pill(
    nav_panel(title = "Chart", plotOutput("chart")), 
    nav_panel(title = "Table", reactableOutput("table"))
  ) # end pills in main panel
  
)


server <- function(input, output, session) {
  
  # create dependent child input
  
  output$select_bev <- renderUI({
    drinks <- starbucks |> 
      filter(Category %in% input$select_cat) |> 
      pull(Drink) |>
      unique() |>
      sort()
    
    selectInput("select_bev", 
                label = "Drink",
                choices = drinks,
                multiple = TRUE)
  })
  
  # create dependent grandchild input
  
  output$select_size <- renderUI({
    sizes <- starbucks |>
      filter(Drink %in% input$select_bev) |>
      pull(Size) |>
      unique() |>
      sort()
    
    selectInput("select_size", "Size",
                choices = sizes,
                multiple = TRUE)
  })
  
  # specify button action
  
  observeEvent(input$reset_button, {
    updateSelectInput(inputId = "select_cat",
                      choices=character(0))
    updateSelectInput(inputId = "select_bev",
                      choices=character(0))
    updateSelectInput(inputId = "select_size",
                      choices=character(0))
  })
  
  # filter dataset based on inputs and make reactive
  
  dataset <- reactive({
    
    if( !is.null(input$select_cat) & !is.null(input$select_bev) & !is.null(input$select_size) ){
      starbucks |>
        filter(Category %in% input$select_cat)|> 
        filter(Drink %in% input$select_bev) |> 
        filter(Size %in% input$select_size)  } else 
          if(!is.null(input$select_cat) & !is.null(input$select_bev) & is.null(input$select_size)){
            starbucks |>
              filter(Category %in% input$select_cat)|> 
              filter(Drink %in% input$select_bev)} else
                if(!is.null(input$select_cat) & is.null(input$select_bev) & is.null(input$select_size)){
                  starbucks |> filter(Category %in% input$select_cat)} else {
                    starbucks
                  }
    
  })
  
  # create chart
  
  output$chart <- renderPlot({
    
    ggplot() +
      geom_point(data = dataset(), 
                 aes(x = Calories, y = `Sugar (g)`, 
                     size = `Caffeine (mg)`, color = Category),
                 alpha = 0.5) +
      scale_size(range = c(0, 10)) + 
      theme_minimal()
    
  })
  
  # create reactable
  
  output$table <- renderReactable({

      reactable(dataset())
    
  })
  
}

shinyApp(ui, server)


# TODO:
# Choose different variable to scale radius.
# Choose color scheme. 
## Use {bslib} theme minty as base.
# Choose different variable to tie to color scheme.
# Fix coordinate system and legends.
# Convert using ggplotly().
# Remove category from legend.


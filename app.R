
# Load packages

packages <- c("shiny", "dplyr", "stringr", "ggplot2", "reactable", "bslib")

invisible(lapply(packages, library, character.only = TRUE))


# Source prep file

source("prep.R")


# Create app itself

ui <- page_sidebar(
  
  theme = bs_theme(preset = "minty",
                   secondary = "#5db69d"),
  
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

      reactable(dataset(),
                groupBy = "Category",
                searchable = TRUE,
                language = reactableLang(searchPlaceholder = "Search the table."),
                resizable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f2f9f7",
                                       highlightColor = "#aedace"))
    
  })
  
}

shinyApp(ui, server)


# TODO:
# Choose different variable to scale radius.
# Choose color scheme. 
## Use {bslib} theme minty as base.
# Tie milk type to color.
# Fix coordinate system and legends.
# Convert using ggplotly().
# Remove category from legend.
# Use shape of mark to represent category.

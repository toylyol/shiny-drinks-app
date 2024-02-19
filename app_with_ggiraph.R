
# Load packages

packages <- c("shiny", "dplyr", "stringr", "ggplot2", "reactable", "bslib", "ggiraph")

invisible(lapply(packages, library, character.only = TRUE))


# Source prep file

source("prep.R")


# Create app itself

ui <- page_sidebar(
  
  theme = bs_theme(preset = "minty",
                   secondary = "#5db69d",
                   danger = "#c2788d"),
  
  title = "Shiny Drinks",
  
  sidebar = sidebar(
    selectInput("select_cat", 
                label = "Category", 
                choices = sort(unique(starbucks$Category)),
                multiple = TRUE,
                selectize = TRUE), # create parent
    uiOutput("select_bev"), # name child
    uiOutput("select_size"), # name grandchild
    actionButton("reset_button", "Reset Filters", class = "btn-danger", icon = icon("trash"))
  ), # end sidebar
  
  navset_pill(
    nav_panel(title = "Chart", girafeOutput("chart")), 
    nav_panel(title = "Table", reactableOutput("table")),
    nav_panel(title = "Help", p(instructions))
  ) # end pills in main panel
  
)


server <- function(input, output, session) {
  
  ## create child input
  
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
  
  ## create grandchild input
  
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
  
  ## specify button action
  
  observeEvent(input$reset_button, {
    updateSelectInput(inputId = "select_cat",
                      choices=character(0))
  })
  
  ## filter dataset based on inputs and make reactive
  
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
  
  ## render interactive chart
  
  output$chart <- renderGirafe({
    
    bubble_chart <- ggplot(data = dataset()) +
      geom_point_interactive(aes(x = Calories, y = `Sugar (g)`, 
                                 size = `Caffeine (mg)`, fill = Category,
                                 tooltip = paste("Category:", Category, 
                                                 "\nDrink:", Drink,
                                                 "\nSize:", Size,
                                                 "\nWhipped Topping:", `Whipped Topping`,
                                                 "\nCalories:", Calories,
                                                 "\nSugar (g):", `Sugar (g)`,
                                                 "\nCaffeine (mg):", `Caffeine (mg)`),
                                 data_id = id),
                             alpha = 0.5,
                             stroke = .25,
                             shape = 21, # ensure shape has color and fill property
                             color = "gray35") + # update stoke color
      scale_fill_manual(values = c("Coffee" = "#77c1ad", 
                                   "Tea" = "#016876", 
                                   "Other" = "#9ae871")) +
      scale_size(range = c(5, 10)) +  # set point size range
      guides(fill = guide_legend(direction = "horizontal",
                                 override.aes = list(size = 10,
                                                     shape = 22), # ensure shape can have fill
                                 order = 1),
             size = guide_legend(direction = "horizontal",
                                 order = 2)) +
      labs(x = "\nCalories",
           y = "Sugar (g)\n",
           fill = "Category: ",
           size = "Caffeine (mg): ") +
      theme_minimal() +
      theme(
        legend.position = "bottom", # put legends at bottom
        legend.box = "vertical", # stack legends
        legend.box.margin=margin(25,0,0,0), # add space between plot and legend
        plot.margin = margin(50, 10, 10, 10), # add margin between pills and plot
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(linewidth = .75, 
                                        linetype = "dotted"),
      ) +
      coord_cartesian(expand = FALSE,
                      clip = "off") 
    
    x <- girafe(ggobj = bubble_chart)
    
    x <- girafe_options(x,
                        opts_toolbar(saveaspng = TRUE,
                                     position = c("top"),
                                     pngname = "shiny-drinks-download"), # set default name
                        opts_zoom(min = 1, max = 5),
                        opts_hover(css = "opacity: 1; stroke-width: 1; cursor:crosshair"),
                        opts_hover_inv(css = "opacity: 0.2;"),
                        opts_selection(only_shiny = FALSE, type = "single") # keep bubble selected on click
    )
    
    if( interactive() ) print(x)
    
  })
  
  ## create reactable
  
  output$table <- renderReactable({

      reactable(dataset(),
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
# Left-align chart.
# Write help page.
# Make chart fullscreen, if possible.
# Bring selected bubble to front, if possible.
# Go back to short-form ggiraph with list of options.
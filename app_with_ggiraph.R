
# Load packages

# library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(reactable)
library(bslib)
library(ggiraph)


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
    nav_panel(title = "Chart",
              tags$div(style = "float:left; padding: 10px;", 
                       girafeOutput("chart", width = "225%", height = "225%"))
              ), # add div to left-align chart
    nav_panel(title = "Table", reactableOutput("table")),
    nav_panel(title = "Help", 
              br(), # create space between pills
              card(
                card_header(class = "bg-secondary",
                            "How do I remove just one filter option instead of resetting all of them?"),
                card_body(
                  markdown(
                    "Click on the option that you want to remove so that it is highlighted. Push [Delete] on your keyboard.")
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "I want to zoom in a specific part of the chart. Is that possible?"),
                card_body(
                  markdown(
                    "When you hover over the chart, a menu appears at the top. Use the second icon from the left (a maginifying glass with a rectangle in the middle) to activate the zoom feature. Once it is activated, click and drag over the part of the chart where you want to zoom in.")
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "Oh no, I zoomed in way too far!"),
                card_body(
                  markdown(
                    "Don't worry: You can reset the chart. Use the icon that looks like four arrows pointing outward toward four corners.")
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "Is there a way to keep the chart from zooming?"),
                card_body(
                  markdown(
                    "Yes! You can disable the zoom by clicking on the first maginifying glass icon in the menu. You will know zoom is disabled when a slash appears through the icon.")
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "Why are bubbles turning red as I'm exploring the chart?"),
                card_body(
                  markdown(
                    "When you click on a bubble, it turns red. Click on the same bubble again to return the bubble to its original color."
                  )
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "Can I download the chart?"),
                card_body(
                  markdown(
                    "Click on the download arrow icon (last one on the right of the menu that appears at the top of the chart when you hover) to save a copy of the chart as it appears on your screen.")
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "Can I make the table columns wider?"),
                card_body(
                  markdown(
                    "Absolutely, hover over the column header you want to move until a symbol that looks like parallel lines with arrows pointing in opposite directions appears. Click and drag the column header until it wide enough for you.")
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "How did you come up with the color scheme?"),
                card_body(
                  markdown(
                    "The Bootswatch minty color scheme available through {bslib} was used to style the app. The bubble chart color scheme was created using Gramazio, Laidlaw, and Schloss' [Colorgorical](http://vrl.cs.brown.edu/color). They wrote a [paper](http://vrl.cs.brown.edu/color/pdf/colorgorical.pdf?v=5dd92af6d1e6c5584236275adc769e82) about it for *IEEE Transactions on Visualization and Computer Graphics* in 2017."
                  )
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "Where did you get this nutrition information?"),
                card_body(
                  markdown(
                    "The data are from Starbucks by way of the [TidyTuesday repository](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-12-21/readme.md).")
                )
              ), # end card
              card(
                card_header(class = "bg-secondary",
                            "How did you make the app?"),
                card_body(
                  markdown(
                    "See the code on [GitHub](https://github.com/toylyol/shiny-drinks-app)!")
                )
              ), # end card
    ) #end help pill
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
                                 data_id = ID),
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
        legend.box.margin=margin(15,0,0,0), # add space between plot and legend
        plot.margin = margin(25, 10, 10, 10), # add margin between pills and plot
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(linewidth = .75, 
                                        linetype = "dotted"),
      ) +
      coord_cartesian(expand = FALSE,
                      clip = "off") 
 
    girafe(
      ggobj = bubble_chart,
      options = list(
        opts_toolbar(
          saveaspng = TRUE,
          position = c("top"),
          pngname = "shiny-drinks-download" # set default name
        ),
        opts_zoom(min = 1, max = 5),
        opts_hover(css = "opacity: 2; stroke-width: 2; stroke: black; cursor:crosshair"),
        opts_hover_inv(css = "opacity: 0.1;"),
        opts_selection(only_shiny = FALSE, type = "single") # keep bubble selected on click
      )
    )
      
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
# Bring selected bubble to front.
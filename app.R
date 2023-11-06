library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "header  header header",
    "sidebar area4  .     ",
    "table   table  plotly",
    "table   table  plotly"
  ),
  row_sizes = c(
    "100px",
    "0.81fr",
    "1.19fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "0.59fr",
    "1.41fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Model", ""),
    card_body(
      selectInput(
        inputId = "mySelectInput",
        label = "Select Input",
        choices = list(
          "Demonstration" = "demo",
          "T-test" = "ttest",
          "ANOVA" = "anova",
          "Mixed-effects" = "lmes",
          "SEM" = "sem"
        ),
        selected = "demo"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Power to the People (by simulation)",
    alignment = "start",
    is_title = TRUE
  ),
  grid_card(
    area = "table",
    card_header("Table"),
    card_body(DTOutput(outputId = "myTable", width = "100%"))
  ),
  grid_card(
    area = "plotly",
    card_header("Interactive Plot"),
    card_body(
      plotlyOutput(
        outputId = "distPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "area4",
    card_body(
      sliderInput(
        inputId = "inputId",
        label = "Slider Input",
        min = 0,
        max = 10,
        value = 5,
        width = "100%",
        step = 1
      )
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
  


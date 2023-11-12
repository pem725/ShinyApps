# Load the shiny and longpower packages
library(shiny)
library(longpower)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  
  # Add title
  titlePanel("Linear Mixed Model Sample Size Calculations"),
  
  # Add layout
  sidebarLayout(
    
    # Input
    sidebarPanel(
      selectInput(
        "typeInput",
        "Use effect size (Cohen's d) or mean difference:",
        choices = c("Effect size", "Mean difference")
      ),
      conditionalPanel(
        condition = "input.typeInput == 'Effect size'",
        numericInput(
          "numDelta",
          "Cohen's d:",
          min = 0,
          step = 0.1,
          value = 0.5
        )
      ),
      conditionalPanel(
        condition = "input.typeInput == 'Mean difference'",
        numericInput(
          "numDelta",
          "Mean difference:",
          min = 0,
          step = 0.1,
          value = 0.5
        ),
        numericInput(
          "sdA",
          "SD for Group A:",
          min = 0,
          value = 1
        ),
        numericInput(
          "sdB",
          "SD for Group B:",
          min = 0,
          value = 1
        )
      ),
      numericInput(
        "numLambda",
        "Group allocation ratio (N1:N2):",
        value = 1,
        min = 0.01,
        step = 0.1
      ),
      numericInput(
        "numTime",
        "Number of time-points:",
        min = 2,
        max = 5,
        value = 2
      ),
      numericInput(
        "corTime",
        "Within-subjects correlation between time-points:",
        min = 0,
        max = 0.99,
        step = 0.1,
        value = 0.5
      ),
      numericInput(
        "numAlpha",
        "Alpha:",
        min = 0,
        max = 1,
        step = 0.01,
        value = 0.05
      ),
      selectInput(
        "sideInput",
        "One- or two-sided test:",
        choices = c("Two-sided", "One-sided")
      ),
      numericInput(
        "numPower",
        "Power:",
        min = 0.8,
        max = 1,
        step = 0.1,
        value = 0.8
      ),
      # Attrition
      strong("Attrition"),
      p("Specify attrition rate between the time-points as a proportion."),
      conditionalPanel(
        condition = "input.numTime >= 2",
        numericInput(
          "numAttr1",
          "Time 1 to 2:",
          min = 0,
          max = 0.99,
          step = 0.1,
          value = 0
        )
      ),
      conditionalPanel(
        condition = "input.numTime >= 3",
        numericInput(
          "numAttr2",
          "Time 2 to 3:",
          min = 0,
          max = 0.99,
          step = 0.1,
          value = 0
        )
      ),
      conditionalPanel(
        condition = "input.numTime >= 4",
        numericInput(
          "numAttr3",
          "Time 3 to 4:",
          min = 0,
          max = 0.99,
          step = 0.1,
          value = 0
        )
      ),
      conditionalPanel(
        condition = "input.numTime == 5",
        numericInput(
          "numAttr4",
          "Time 4 to 5:",
          min = 0,
          max = 1,
          step = 0.1,
          value = 0
        )
      )
    ),
    
    # Output
    mainPanel(
      h2("Overview"),
      tags$p("This function calulates the sample size for a mixed model of repeated measures with a compound symmetry correlation structure."),
      tags$p("It uses the", tags$code("power.mmrm"), "function from the", tags$a(tags$code("longpower"), href = "https://cran.r-project.org/web/packages/longpower/index.html"), "package. See Lu, Luo, & Chen (2008) for more information."),
      tags$p(tags$strong("Important information:"), "The calculations assume that the correlation structure and attrition rate are the same for both groups."),
      h2("Results"),
      verbatimTextOutput("console"),
      h3("References"),
      tags$p("Lu, K., Luo, X., Chen, P.-Y. (2008) Sample size estimation for repeated measures analysis in randomized clinical trials with missing data.", tags$i("International Journal of Biostatistics"), "4, (1)"),
      h4("Author"),
      tags$p("This application is authored by Sam Mancuso:", tags$code("sgmancuso <at> gmail <dot> com"))
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  # Calculate required sample size
  
  powerout <- reactive({
    
    rho <- as.numeric(input$corTime)
    k <- as.numeric(input$numTime)
    lambda <- as.numeric(input$numLambda)
    
    # Correlation matrix
    Ra <- matrix(
      rho,
      nrow = k,
      ncol = k
    )
    
    diag(Ra) <- 1
    
    # Delta
    delta <- as.numeric(input$numDelta)
    
    # Standard deviations
    if (input$typeInput == "Effect size") {
      sigmaa <- 1
      sigmab <- 1
    } else {
      sigmaa <- as.numeric(input$sdA)
      sigmab <- as.numeric(input$sdB)
    }
    
    power <- as.numeric(input$numPower)
    sig.level <- as.numeric(input$numAlpha)
    alternative <- ifelse(
      input$sideInput == "Two-sided",
      "two.sided",
      "one.sided"
    )
    
    # Attrition vector
    ra <- vector("numeric", length = k)
    
    for (i in 1:k) {
      if (i == 1) {
        ra[i] <- 1
      } else {
        ra[i] <- 1 - input[[paste0("numAttr", i - 1)]]
      }
    }
    
    samp <- power.mmrm(
      N = NULL,
      Ra = Ra,
      Rb = Ra,
      ra = ra,
      rb = ra,
      delta = delta,
      sigmaa = sigmaa,
      sigmab = sigmab,
      power = power,
      sig.level = sig.level,
      lambda = lambda,
      alternative = alternative
    )
    
    return(samp)
  })
  
  # Display output from powr.mrmm
  output$console <- renderPrint({
    powerout()
  })
  
}

shinyApp(ui, server)
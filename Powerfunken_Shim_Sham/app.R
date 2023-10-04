#
# The Ever Elusive Powerfunken Shim Sham: A power analysis simulation program
#
# Written by the good folks at MRES
# http://mres-gmu.org
#
# Primary Contact:  Patrick E. McKnight
# github: pem725
#

library(shiny)

## Let's setup some control features here

## options for models
modsAvail <- c("t-test", "ANOVA", "Regression", "Generalized Linear Model", "LMER", "CFA", "SEM")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pick Your Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("modSel",
                        "Select the Model that Best Fits YOUR Needs",
                        modsAvail)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

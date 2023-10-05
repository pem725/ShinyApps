#
# The Ever Elusive Powerfunken Shim Sham: A power analysis simulation program
#
# Written by the good folks at MRES
# http://mres-gmu.org
#
# Primary Contact:  Patrick E. McKnight
# github: pem725
#

# RS: 10/5/23 @ 5:11am -  continue on with front end
#     start with the actual sampling soon.

library(shiny)
## library(shinymaterial) # too much work and refactoring - KISS
library(shiny.semantic)
# https://github.com/Appsilon/shiny.semantic
# https://fomantic-ui.com/

## Let's setup some control features here

## options for models
modsAvail <- c("t-test", "ANOVA", "Regression", "Generalized Linear Model", "LMER", "CFA", "SEM")


# Define UI for application that draws a histogram
ui <- material_page(

    # Application title
    titlePanel("Power Estimates by Simulation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("modSel",
                        "Select the Model that Best Fits YOUR Needs",
                        modsAvail),
            img(src="MRES2wbg.png",height=72), "Product of the ", span("MRES lab", style="color:blue")
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
        
        # draw the histogram with the specified number of bins
        hist(rnorm(1000), col = 'darkgray', border = 'white',
             xlab = 'Some random normal value (mu=0, sigma=1, N = 1000)',
             main = 'Histogram')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

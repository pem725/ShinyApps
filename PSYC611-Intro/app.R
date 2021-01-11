#
# ShinyApp 1 for PSYC 611
# In this app, I wish to demonstrate the following (from the syllabus):
# Notation, Levels of Measurement, Distributions 
# ✷ define terms 
# ✷ differentiate scales (H) 
# ✷ identify and produce univariate and sampling distributions (SW) 
# ✷ produce univariate plots (SW) 
# ✷ produce bivariate plots (SW) 

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Levels of Measurement and Distributions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("N",
                        "Sample size:",
                        min = 1,
                        max = 500,
                        value = 50),
            radioButtons("lvl",
                         "Level of Measurement:",
                         c("Nominal" = "nom",
                           "Ordinal" = "ord",
                           "Interval" = "int",
                           "Ratio" = "rat")),
            img(src="MRES2wbg.png",height=72),
            "Product of the ",
            span("MRES lab", style="color:blue") # can I make this clickable and send the user to the MRES website in a new tab/window?
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
        ### Recode local vars here
        N <- input$N
        lvl <- input$lvl
        
        ### Generate data and create a histogram in ggplot
        if (lvl == "nom"){
            x <- data.frame(id = 1:N, x = as.factor(sample(c("Dog","Cat","Fish","Rodent","Bird"), N, replace=TRUE, prob=c(0.5, 0.2, 0.1, 0.1,.1))))
            p.nom <- ggplot(x,aes(x)) + geom_bar(colour = "black", fill = "#56B4E9") +
                theme(axis.line = element_line(size=1, colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  plot.title=element_text(size = 20, family="xkcd-Regular"),
                  text=element_text(size = 16, family="xkcd-Regular"),
                  axis.text.x=element_text(colour="black", size = 12),
                  axis.text.y=element_text(colour="black", size = 12)) +
                ggtitle("Bar plot (Frequency) of a categorical variable") +
                xlab("Variable Levels") +
                ylab("Frequency of each level")
            plot(p.nom)
        }
        if (lvl == "ord"){
            x <- data.frame(id = 1:N, x = round(runif(N,min=1,max=5)))
            p.ord <- ggplot(x,aes(x)) + geom_bar(colour = "black", fill = "#56B4E9") +
                theme(axis.line = element_line(size=1, colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      plot.title=element_text(size = 20, family="xkcd-Regular"),
                      text=element_text(size = 16, family="xkcd-Regular"),
                      axis.text.x=element_text(colour="black", size = 12),
                      axis.text.y=element_text(colour="black", size = 12)) +
                ggtitle("Bar plot (Frequency) of an ordinal variable") +
                xlab("Variable Levels") +
                ylab("Frequency of each level")
            plot(p.ord)
        }
        if (lvl == "int"){
            x <- data.frame(id = 1:N, x = rnorm(N,mean=100,sd=15))
            p.int <- ggplot(x,aes(x)) + geom_histogram(colour = "black", fill = "#56B4E9") +
                theme(axis.line = element_line(size=1, colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      plot.title=element_text(size = 20, family="xkcd-Regular"),
                      text=element_text(size = 16, family="xkcd-Regular"),
                      axis.text.x=element_text(colour="black", size = 12),
                      axis.text.y=element_text(colour="black", size = 12)) +
                ggtitle("Histogram of an interval level variable") +
                xlab("General Cognitive Ability (IQ)") +
                ylab("Frequency Observed in Sample")
            plot(p.int)
        }
        if (lvl == "rat"){
            x <- data.frame(id = 1:N, x = round(rpois(N,lambda=1)))
            p.ord <- ggplot(x,aes(x)) + geom_histogram(colour = "black", fill = "#56B4E9") +
                theme(axis.line = element_line(size=1, colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      plot.title=element_text(size = 20, family="xkcd-Regular"),
                      text=element_text(size = 16, family="xkcd-Regular"),
                      axis.text.x=element_text(colour="black", size = 12),
                      axis.text.y=element_text(colour="black", size = 12)) +
                ggtitle("Histogram of a ratio level variable") +
                xlab("Times went to the dentist during graduate school") +
                ylab("Frequency Observed in Sample")
            plot(p.ord)
        }
        
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

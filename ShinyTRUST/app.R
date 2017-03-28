#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Trust Model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Nperc",
                     "Sample Size (%):",
                     min = 1,
                     max = 100,
                     value = 100),
         checkboxGroupInput("source",
                            "Subjects to Include:",
                            c("Psychology Undergrads" = "SONA",
                              "Mechanical Turk Workers" = "mTurk",
                              "Reddit Readers" = "Reddit",
                              "GMU Community Members" = "Flyers"),
                            selected = "SONA"),
         checkboxGroupInput("study",
                            "Studies to Include:",
                            c("Study 3" = 3,
                              "Study 4" = 4,
                              "Study 5" = 5),
                            selected = 5) #,
        #radioButtons("boot", "Bootstrap Results?:",
        #            c("Yes" = "y",
        #              "No" = "n")),
        # conditionalPanel(condition = "input.boot == 'y'",
        #                  sliderInput("reps","Number of Samples:", min = 1, max = 10000, value=1000))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Plots")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   library(ggplot2)
   library(gridExtra)
   output$Plots <- renderPlot({
     dat <- read.csv("AllTrustShiny.csv")
     dat <- dat[dat$source %in% input$source & dat$study %in% input$study,]
     x <- dat[sample(1:nrow(dat),round((input$Nperc/100)*nrow(dat))),] # random sample 
     model <- lm(T~G:U1:R,data=x)
     x.l <- reshape(x,varying=names(x[c(3:6,9)]),new.row.names=1:(nrow(x)*5),v.names="value",timevar="Measure",times=c("G","U1","U2","R","U3"),idvar=c("id","scen"),direction="long")
     dat.hat <- data.frame(G=c(9,9,2,2,9,9,2,2),R=c(9,2,9,2,9,2,9,2),U1=c(9,9,9,9,2,2,2,2))
     lm.hat <- predict(model,newdata = dat.hat,se.fit=T)
     dat.hat <- cbind(dat.hat,lm.hat$fit,lm.hat$se.fit)
     dat.hat$U1 <- factor(dat.hat$U1,labels=c("L","H"))
     #print(summary(model))
     p1 <- ggplot(x.l,aes(x=T,y=value,colour=Measure)) + geom_smooth()
     p2 <- ggplot(x.l,aes(x=T,y=value,colour=Measure)) + geom_smooth() + facet_wrap(~scen)
     dat.hat$G <- factor(dat.hat$G,labels=c("Low","High"))
     dat.hat$R <- factor(dat.hat$R,labels=c("Low","High"))
     names(dat.hat) <- c("G","R","U","T","T.se")
     Unc.labels <- c(L = "Low Uncertainty",H="High Uncertainty")
     p3 <- ggplot(dat.hat,aes(x=factor(G),y=T,colour=factor(R), ymin=T-2*T.se, ymax=T+2*T.se)) + geom_point(size=2) + facet_wrap(~U, labeller = labeller(U=Unc.labels)) + guides(col=guide_legend(title="Reliance")) + xlab("Goal Importance") + ylab("Overall Trust Rating (0-10)") + ylim(c(0,10)) + geom_errorbar(width=.2,col="black")
     p4 <- ggplot(x.l,aes(x=as.factor(scen),y=value,colour=Measure))+geom_boxplot() + xlab("Scenario") + ylab("Level (0-10)")
     grid.arrange(p1, p2, p3, p4)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


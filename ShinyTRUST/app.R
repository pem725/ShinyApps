################################################################################
##                                                                            ## 
##                             ShinyTRUST                                     ##
##                                                                            ##
################################################################################

library(shiny)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(gt)
library(gtsummary)

# Load Data
dat <- read.csv("./AllTrustShiny.csv")

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
                            selected = 5)), #,
        #radioButtons("boot", "Bootstrap Results?:",
        #            c("Yes" = "y",
        #              "No" = "n")),
        # conditionalPanel(condition = "input.boot == 'y'",
        #                  sliderInput("reps","Number of Samples:", min = 1, max = 10000, value=1000))
        #),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Model Summary",tableOutput("lmModel")),
          tabPanel("Model Curves",plotOutput("p1")),
          tabPanel("Vignette Curves",plotOutput("p2")),
          tabPanel("Interaction Plot",plotOutput("p3")),
          tabPanel("Measure by Vignette",plotOutput("p4"))#,
          #tabPanel("All Plots",plotOutput("p5"))
                   )
      )
   )
)

input <- list(source=c("SONA"),study=c(3,4,5), Nperc=c(50))

# Define server logic required to draw a histogram
server <- function(input, output){
##### RESUMPTION CUE --------------------

  ## These data.frames do not need to be created.  We just need to produce temp
  ## data and then pass them to the plot functions.  So, I just need to put all
  ## the conditional computations together and then produce only what we need to
  ## plot.  Save nothing.  No objects except for x and x.l but x is only use for
  ## the interaction plot (p3)
  
  
  x <- reactive({
    req(input$source, input$study, input$Nperc)
    dat %>% filter(
      source %in% input$source,
      study %in% input$study
    ) %>% sample_frac(
      prop = input$Nperc/100
    )
     #dat <- read.csv("AllTrustShiny.csv")
     #d1 <- dat[dat$source %in% input$source & dat$study %in% input$study,]
     #d1[sample(1:nrow(d1),round((input$Nperc/100)*nrow(d1))),]
  })
   
   x.l <- reactive({
     req(input$source, input$study, input$Nperc)
     xl <- dat %>% filter(
       source %in% input$source,
       study %in% input$study
     ) %>% sample_frac(
       prop = input$Nperc/100
     )
     reshape(xl,varying=names(xl[c(3:6,9)]),new.row.names=1:(nrow(xl)*5),v.names="value",timevar="Measure",times=c("G","U1","U2","R","U3"),idvar=c("id","scen"),direction="long")
   })
   
   output$lmModel <- render_gt({
     lm(T~G*U1*R,data=x()) %>%
       tbl_regression(label=list(G ~ "Goal Importance", U1 ~ "Outcome Uncertainty (P(goal))", R ~ "Reliance upon agent")) %>%
       as_gt()
   })

   #print(summary(model))
   #p1 <- ggplot(xl.l,aes(x=T,y=value,colour=Measure)) + geom_smooth()
   #p2 <- ggplot(xl.l,aes(x=T,y=value,colour=Measure)) + geom_smooth() + facet_wrap(~scen)
   output$p1 <- renderPlot({
     ggplot(x.l(),aes(x=value,y=T,colour=Measure)) + geom_smooth()
   })
     
   output$p2 <- renderPlot({
     ggplot(x.l(),aes(x=value,y=T,colour=Measure)) + geom_smooth() + facet_wrap(~scen)
   })

   output$p3 <- renderPlot({
     lm.out <- lm(T~G:U1:R,data=x())
     tmp <- data.frame(G=c(9,9,2,2,9,9,2,2),R=c(9,2,9,2,9,2,9,2),U1=c(9,9,9,9,2,2,2,2))
     lm.hat <- predict(lm.out,newdata = tmp,se.fit=T)
     tmp <- cbind(tmp,lm.hat$fit,lm.hat$se.fit)
     tmp$U1 <- factor(tmp$U1,labels=c("L","H")) # easier - less typing below
     tmp$G <- factor(tmp$G,labels=c("Low","High"))
     tmp$R <- factor(tmp$R,labels=c("Low","High"))
     names(tmp) <- c("G","R","U","T","T.se")
     #browser()
     ggplot(tmp,aes(x=factor(G),y=T,colour=factor(R), ymin=T-2*T.se, ymax=T+2*T.se)) + 
       geom_point(size=2) + 
       facet_wrap(~U, labeller = labeller(U=c(L = "Low Uncertainty",H="High Uncertainty"))) + 
       guides(col=guide_legend(title="Reliance")) + 
       xlab("Goal Importance") + 
       ylab("Overall Trust Rating (0-10)") + 
       ylim(c(0,10)) + 
       geom_errorbar(width=.2,col="black")
   })
  # browser()
   
   output$p4 <- renderPlot({
     ggplot(x.l(),aes(x=as.factor(scen),y=value,colour=Measure))+geom_boxplot() + xlab("Scenario") + ylab("Level (0-10)")
   })
     
   # output$p5 <- renderPlot({grid.arrange(p1, p2, p3, p4)})
}

# Run the application 
shinyApp(ui = ui, server = server)


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
#library(shiny.semantic)
# https://github.com/Appsilon/shiny.semantic
# https://fomantic-ui.com/

library('paramtest')
library('pwr')
library('ggplot2')
library('knitr')
library('nlme')
library('lavaan')
library('dplyr')
library(bslib) # the prime driver here for UI
### now do the sim stuff
# create user-defined function to generate and analyze data

## functions used later
t_func <- function(simNum, N, d) {
  x1 <- rnorm(N, 0, 1)
  x2 <- rnorm(N, d, 1)
  t <- t.test(x1, x2, var.equal=TRUE)  # run t-test on generated data
  stat <- t$statistic
  p <- t$p.value
  return(c(t=stat, p=p, sig=(p < .05)))
  # return a named vector with the results we want to keep
}

## run_test is our simulation engine

#power_ttest <- run_test(t_func, n.iter=5000, output='data.frame', N=50, d=.5)  # simulate data

#results(power_ttest) %>% summarise(power=mean(sig))

#power_ttest_vary <- grid_search(t_func, params=list(N=c(25, 50, 100)),
#                                n.iter=5000, output='data.frame', d=.5)
#results(power_ttest_vary) %>%
#  group_by(N.test) %>%
#  summarise(power=mean(sig))

# varying N and Cohen's d
#power_ttest_vary2 <- grid_search(t_func, params=list(N=c(25, 50, 100), d=c(.2, .5)),
#                                 n.iter=5000, output='data.frame')
#power <- results(power_ttest_vary2) %>%
#  group_by(N.test, d.test) %>%
#  summarise(power=mean(sig))
#print(power)
#ggplot(power, aes(x=N.test, y=power, group=factor(d.test), colour=factor(d.test))) +
#  geom_point() +
#  geom_line() +
#  ylim(c(0, 1)) +
#  labs(x='Sample Size', y='Power', colour="Cohen's d") +
#  theme_minimal()


## Let's setup some control features here

## options for models
modsAvail <- c("Demonstration","t-test")#, "ANOVA", "Regression", "Generalized Linear Model", "LMER", "CFA", "SEM")


# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Power to the People",
  theme = bs_theme(bootswatch = "minty"),
  sidebar = sidebar(
    selectInput("modsAvail","Select Stat Model", modsAvail),
    conditionalPanel(
      condition = "input.modsAvail == 'Demonstration'",
      sliderInput("Ndemo","Sample Size", min=0, max=1000, value=150, step=50, animate=T),
      sliderInput("MUdemo", "Mean", min = 0, max = 15, value=5, step=.25, animate=T),
      sliderInput("SDdemo", "SD", min = 0, max = 15, value=5, step=.25, animate=T),
      sliderInput("Rel","Reliability of Measure", min=0, max=1, value = .8, step = .01, animate = T)
    ),
    conditionalPanel(
      condition = "input.modsAvail == 't-test'",
      sliderInput("ExpES","Expected Effect size", min=0,max=3,value=.1,step=.1,animate = T),
      sliderInput("pCrit","Alpha level?",min=.001,max=.1,value=.05,step=.01,animate=T),
      sliderInput("ExpN","Expected Sample Size", min=2, max=1500, value=250, step=20, animate=T)
      )#,
    #conditionalPanel(
    #  condition = "input.modsAvail == 'ANOVA'",
    #  
    #)
  ),
  layout_columns(
    fill = FALSE,
    
  value_box(
      title = "Statistical Power (a priori)",
      value = textOutput("powerpe"),
      showcase = bsicons::bs_icon("battery-charging"),
      theme = "teal"    
  ),
  value_box(
    title = "Statistical Power (post-hoc)",
    value = textOutput("powerph"),
    showcase = bsicons::bs_icon("battery-charging"),
    theme = "pink"  
  )),
  #cards <- list(
  card(
      full_screen = TRUE,
      card_header("A one-group t-test"),
      plotOutput("p1")  
    )
  
    
  #)

  
  
    # Application title
    #titlePanel("Power Estimates by Simulation"),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         radioButtons("modSel",
    #                     "Select the Model that Best Fits YOUR Needs",
    #                     modsAvail),
    #         img(src="MRES2wbg.png",height=72), "Product of the ", span("MRES lab", style="color:blue")
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot")
    #     )
    # ) 
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  genDat <- reactive({
    if(input$modsAvail == "Demonstration"){
      data.frame(obs=1:input$Ndemo, x=rnorm(input$Ndemo, input$MUdemo, sqrt((input$SDdemo^2)/input$Rel)))
    }
  })
  
  output$p1 <- renderPlot({
      # draw the histogram with the specified number of bins
      ggplot(genDat(),aes(x=x)) + 
         geom_histogram(aes(y = ..density..)) +
         geom_density(fill = "#56B4E9", alpha=.2) +
         geom_vline(aes(xintercept = 0), color = "blue", linetype = "dashed", size = 1) +
         geom_vline(aes(xintercept = mean(x)), color = "red", size = 1) +
         geom_rect(aes(xmin = (mean(x) - 1.97*sd(x)/sqrt(length(x))), xmax = (mean(x) + 1.97*sd(x)/sqrt(length(x))), ymin=0, ymax = Inf), alpha = .01, fill = "pink")
  })
  
  output$powerpe <- renderText({ # power point estimate
    round(power.t.test(input$Ndemo, input$MUdemo, input$SDdemo)$power,2)
  })
  
  output$powerph <- renderText({ # power point estimate
    round(power.t.test(input$Ndemo, input$MUdemo, sqrt((input$SDdemo^2)/input$Rel))$power,2)
  })
  
} # end server section

# Run the application 
shinyApp(ui = ui, server = server)

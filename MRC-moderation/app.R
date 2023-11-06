#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(markdown)
library(DT)
library(ggfortify) # see:  https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_lm.html
#devtools::install_github("jacob-long/jtools")
library(jtools)
#library(fabricatr)
library(gtsummary)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#devtools::install_github("ggobi/ggally")
library(GGally)

## some functions

GenModData <- function(N,b0,b1,b2,b3,R2,coding){
  x <- rnorm(N)
  m <- gl(2,N/2,labels = c("lo","hi"))
  
  if (coding=="dummy"){
    m <- c(rep(0,N/2),rep(1,N/2))
  } else if (coding=="effects"){
    m <- c(rep(0,N/2),rep(1,N/2))
  } 
  
  
  m <- 

  y <-   
  y.hat <- rowSums(xw)
  err1 <- rnorm(N, mean(y.hat), sd(y.hat)) # random error
  y.obs <- scale(y.hat)*sqrt(R2) + scale(residuals(lm(err1~y.hat))) * sqrt(1-R2)
  out <- data.frame(Y=y.obs,pdat)  
    
  
}

GenDataReg <- function(N,Np,Nc,COR,Wts,R2,Outls=NULL){ ## where N=sample size, Np=num of pred, Nc=num of common factors of predictors, COR=avg cor among pred, Wts=regression wts, R2=rel bet yhat and yobs
  # first create the predictor variables  
  
  if (Np==1) {
    pdat <- data.frame(P1=rnorm(N,0,1))
  } else {
    NV <- round((Np/Nc)) ## evenly distributed number of vars per underlying factor
    pdat <- data.frame(id=1:N)
    for(i in 1:Nc){
      cormat <- matrix(rbinom(NV*NV,100,COR)/100,NV,NV)
      diag(cormat) <- 1
      U <- t(chol(cormat))
      random.normal <- matrix(rnorm(NV*N,0,1),NV,N)
      tmp2 <- as.data.frame(t(U %*% random.normal))
      names(tmp2) <- paste("P",1:NV,i,sep="")
      pdat <- cbind(pdat,tmp2)
    }
    pdat <- pdat[,-1]
  }
  
  ## now created the Y vars
  if (is.null(Wts)){
    Wts <- rep(1,ncol(pdat))
  } else if (length(Wts) != ncol(pdat)){
    Wts <- rep(1,ncol(pdat))
  }
  xw <- pdat
  for (i in 1:length(Wts)){
    xw[,i] <- pdat[,i]*Wts[i]
  }
  y.hat <- rowSums(xw)
  err1 <- rnorm(N, mean(y.hat), sd(y.hat)) # random error
  y.obs <- scale(y.hat)*sqrt(R2) + scale(residuals(lm(err1~y.hat))) * sqrt(1-R2)
  out <- data.frame(Y=y.obs,pdat)  
  
  if (is.null(Outls) | Outls == 0){
    fin <- out
  } else {
    Oobs <- sample(1:N, Outls)
    for (i in Oobs){
      OobsCol <- sample(1:ncol(out),1)
      out[i,OobsCol] <- out[i,OobsCol]*5
    }
    fin <- out
  }
  
  return(fin)
}

# makeMyDat <- function(N, rxx, nvar, Mc){
#   manipYeq <- function(N,rxx){
#     y.hat <- rnorm(N)
#     err1 <- rnorm(N)
#     y.obs <- scale(y.hat)*sqrt(rxx) + scale(residuals(lm(err1~y.hat))) * sqrt(1-rxx)
#     dat <- data.frame(y.obs,y.hat)
#     return(dat)
#   }
#   
#   makeXvars <- function(dat, nvar){
#     if (nvar==1){
#       out <- data.frame(Y = dat$y.obs, X = dat$y.hat)
#     }
#     if (nvar==2){
#       X1 <- rnorm(nrow(dat))
#       X2 <- dat$y.hat - X1
#       out <- data.frame(Y = dat$y.obs, X1 = X1, X2 = X2)
#     }
#     if (nvar==3){
#       X1 <- rnorm(nrow(dat))
#       X2 <- rnorm(nrow(dat))
#       X3 <- dat$y.hat - X1 - X2
#       out <- data.frame(Y=dat$y.obs, X1 = X1, X2 = X2, X3 = X3)
#     }
#     return(out)
#   }
#   tmp <- manipYeq(N, rxx)
#   ret <- makeXvars(tmp, nvar)
#   return(ret)
# }

## test above
#input <- data.frame(N=100, Rsq=.5, Np=3)
#MyDat <- makeMyDat(input$N, input$Rsq, input$Np)
#lm1 <- lm(Y~.,MyDat)
#print(tbl_regression(lm1))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Demonstrating Moderation in MRC"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Basic Inputs"),
            sliderInput("N",
                        "Sample Size:",
                        min = 10,
                        max = 1000,
                        value = 100),
            sliderInput("Rsq",
                        "Model fit (% of variance in Y explained):",
                        min=0,
                        max=1,
                        value=.3), ## stopped here
            radioButtons("Nc",
                        "Clusters for predictors",
                        choices=c("One Large Mess" = 1,
                                  "Two Groups of Predictors" = 2)),
            radioButtons("Wts",
                         "Regression Weights:",
                         choices=c("Uniform (all equal)" = 1,
                                   "Ascending" = 2,
                                   "Random" = 3)),
            sliderInput("Outls",
                        "Number of Outliers:",
                        min=0,
                        max=10,
                        value=0)
            #radioButtons("dist", 
            #             label = h3("Distribution of DV"),
            #             choices = list("Normal" = 1, "Binary" = 2, "Random" = 3),
            #             selected = 1)
            #radioButtons("ml",
            #             label = h3("Create Nested Data?"),
            #             choices = list("Yes" = 1, "No" = 0),
            #            selected = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("Model Summary", DT::dataTableOutput("summary")),
             tabPanel("Standard Plot", plotOutput("MRCPlot")),
             tabPanel("Bivariate Plots", plotOutput("biPlots")),
             tabPanel("Diagnostic Plots", plotOutput("Diags"))
        ) # end tabsetPanel
    ) # end MainPanel
  ) # end sidebarLayout
) # end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## take the radio button inputs and create the necessary values for the GenDataReg below
  
  Wts <- reactive({
    if (input$Wts == 1) {
      rep(1,input$Np)
    } else if (input$Wts == 2) {
      seq(1:input$Np)
    } else if (input$Wts == 3) {
      rnorm(input$Np,2,2)
    }
  })
  
  MyDat <- reactive({
    GenDataReg(N=input$N, 
               Np=as.numeric(input$Np), 
               Nc=as.numeric(input$Nc), 
               COR=input$Mc, 
               Wts=Wts(), 
               R2=input$Rsq, 
               Outls=input$Outls)
  })
  
  ## for plot and table ideas, see:
  ##  1.  https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html
  ##  2.  https://jtools.jacob-long.com/
  ##  3.  https://jjallaire.shinyapps.io/shiny-ggplot2-brushing/
  ##  4.  https://stackoverflow.com/questions/57584823/how-to-maintain-decimal-point-rounding-of-export-summs-in-a-r-shiny-app
  ##  5.  
  
  MyReg <- reactive({
    lm(Y~., data=MyDat())
  })
   
  # getPage <- function(){
  #   return(includeHTML("regtable.html"))
  # }
  # 
    output$summary <- DT::renderDataTable({
      round(as.data.frame(summary(MyReg())$coefficients),3)
      #includeHTML("regtable.html")
    }, options = list(dom = 't'))

    output$MRCPlot <- renderPlot({
      localD <- MyDat()
      localD$res <- resid(MyReg())
      localD$Yhat <- predict(MyReg())
      ggplot(localD, aes(Yhat, Y)) + geom_smooth() + geom_abline(col="red",lwd=2,lty=2)
    })
    
    output$biPlots <- renderPlot({
      localD <- MyDat()
      localD$res <- resid(MyReg())
      localD$Yhat <- predict(MyReg())
      ggpairs(localD)
    })
    
    output$Diags <- renderPlot({
      autoplot(MyReg())
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

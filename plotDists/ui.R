shinyUI(
  pageWithSidebar(
    headerPanel("Plot different distributions"),
    
    sidebarPanel(
      selectInput("Distribution","Please Select Distribution Type",
                  choices=c("Beta","Binomial","Cauchy","ChiSquare",
                            "Exponential","F","Gamma","Geometric",
                            "Hypergeometric","Log-Normal","Negative Binomial",
                            "Normal","Poisson","t","Uniform","Weibull")),
      sliderInput("sampleSize","Please Select Sample Size: ",
                  min=10,max=5000,value=100,step=10),
      conditionalPanel(condition = "input.Distribution=='Beta'",
                       textInput("a","Please enter the a value",value=1),
                       textInput("b","Please enter the b value",value=1),
                       textInput("ncp","Please enter the non-centrality parameter",value=0)),
      conditionalPanel(condition = "input.Distribution=='Binomial'",
                       textInput("trials","Please enter the number of trials",value=1),
                       textInput("prob","Please enter the probability",value=.5)),
      conditionalPanel(condition = "input.Distribution=='Cauchy'",
                       textInput("location","Please enter the location parameter",value=0),
                       textInput("scale","Please enter the scale parameter",value=1)),
      conditionalPanel(condition = "input.Distribution=='ChiSquare'",
                       textInput("df","Please enter the degrees of freedom",value=1),
                       textInput("ncp","Please enter the noncentrality parameter",value=0)),
      conditionalPanel(condition="input.Distribution=='Exponential'",
                       textInput("lambda","Please enter the exponential lambda",1)),
      conditionalPanel(condition = "input.Distribution=='F'",
                       textInput("df1","Please enter the first df value",value=1),
                       textInput("df2","Please enter the second df value",value=1),
                       textInput("ncp","Please enter the noncentrality parameter",value=0)),
      conditionalPanel(condition = "input.Distribution=='Gamma'",
                       textInput("shape","Please enter the shape value",value=1),
                       textInput("scale","Please enter the scale value (scale=1/rate)",value=1)),
      conditionalPanel(condition = "input.Distribution=='Geometric'",
                       textInput("prob","Please enter the probability",value=.5)),
      conditionalPanel(condition = "input.Distribution=='Hypergeometric'",
                       textInput("m","Please enter the number of white balls in urn",value=1),
                       textInput("n","Please enter the number of black balls in urn",value=1),
                       textInput("k","Please enter the number of balls drawn from urn",value=1)),
      conditionalPanel(condition = "input.Distribution=='Log-Normal'",
                       textInput("meanlog","Please enter the mean log value",value=0),
                       textInput("sdlog","Please enter the sd log value",value=1)),
      conditionalPanel(condition = "input.Distribution=='Negative Binomial'",
                       textInput("size","Please enter the number of successful trials",value=1),
                       textInput("prob","Please enter the probability of sccess in each trial",value=.5)),
      conditionalPanel(condition = "input.Distribution=='Normal'",
                       textInput("mean","Please enter the mean",value=0),
                       textInput("sd","Please enter the standard deviation",value=1)),
      conditionalPanel(condition = "input.Distribution=='Poisson'",
                       textInput("lambda","Please enter the lambda value",value=1)),
      conditionalPanel(condition = "input.Distribution=='t'",
                       textInput("df","Please enter the df",value=1),
                       textInput("ncp","Please enter the noncentrality parameter",value=0)),
      conditionalPanel(condition = "input.Distribution=='Uniform'",
                       textInput("min","Please enter the minimum value in the range",value=0),
                       textInput("max","Please enter the maximum value in the range",value=1)),
      conditionalPanel(condition = "input.Distribution=='Weibull'",
                       textInput("shape","Please enter the shape parameter (a)",value=1),
                       textInput("scale","Please enter the scale parameter (b)",value=1)),
      img(src="MRES2wbg.png",height=72),
      "Product of the ",
      span("MRES lab", style="color:blue")
      ),
  mainPanel(plotOutput("myPlot"))
  )
)
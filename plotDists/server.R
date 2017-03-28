 shinyServer(
   function(input,output){
     output$myPlot <- renderPlot({
       distType <- input$Distribution
       size <- as.numeric(input$sampleSize)
       if(distType=="Beta"){
         randVec <- rbeta(size,shape1=as.numeric(input$a),shape2=as.numeric(input$b),ncp=as.numeric(input$ncp))
       } 
       if(distType=="Binomial"){
         randVec <- rbinom(size,size=as.numeric(input$trials),prob=as.numeric(input$prob))
       } 
       if(distType=="Cauchy"){
         randVec <- rcauchy(size,location=as.numeric(input$location),scale=as.numeric(input$scale))
       } 
       if(distType=="ChiSquare"){
         randVec <- rchisq(size,df=as.numeric(input$df),ncp=as.numeric(input$ncp))
       } 
       if(distType=="Exponential"){
         randVec <- rexp(size,rate=1/as.numeric(input$lambda))
       } 
       if(distType=="F"){
         randVec <- rf(size,df1=as.numeric(input$df1),df2=as.numeric(input$df2),ncp=as.numeric(input$ncp))
       } 
       if(distType=="Gamma"){
         randVec <- rgamma(size,shape=as.numeric(input$shape),scale=as.numeric(input$scale))
       }
       if(distType=="Geometric"){
         randVec <- rgeom(size,prob=as.numeric(input$prob))
       }
       if(distType=="Hypergeometric"){
         randVec <- rhyper(size,m=as.numeric(input$m),n=as.numeric(input$n),k=as.numeric(input$k))
       }
       if(distType=="Log-Normal"){
         randVec <- rlnorm(size,meanlog=as.numeric(input$meanlog),sdlog=as.numeric(input$sdlog))
       }
       if(distType=="Negative Binomial"){
         randVec <- rnbinom(size,size=as.numeric(input$size),prob=as.numeric(input$prob))
       }
       if(distType=="Normal"){
         randVec <- rnorm(size,mean=as.numeric(input$mean),sd=as.numeric(input$sd))
       }
       if(distType=="Poisson"){
         randVec <- rpois(size,lambda=as.numeric(input$lambda))
       }
       if(distType=="t"){
         randVec <- rt(size,df=as.numeric(input$df),ncp=as.numeric(input$ncp))
       }
       if(distType=="Uniform"){
         randVec <- runif(size,min=as.numeric(input$min),max=as.numeric(input$max))
       }
       if(distType=="Weibull"){
         randVec <- rweibull(size,shape=as.numeric(input$shape),scale=as.numeric(input$scale))
       }
       hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
     })
   }
)
 shinyServer(
   function(input,output){
     library(ggplot2)
     output$myPlot <- renderPlot({
       distType <- input$Distribution
       size <- as.numeric(input$sampleSize)
       if(distType=="Beta"){
         randVec <- data.frame(x=rbeta(size,shape1=as.numeric(input$a),shape2=as.numeric(input$b),ncp=as.numeric(input$ncp)))
         plot(ggplot(randVec,aes(x=x)) + geom_histogram(aes(y = ..density..), binwidth=density(randVec$x)$bw) + geom_density(fill="blue", alpha=0.3) + theme_bw() + xlab('') + ylab('') + xlim(c(0,1)))
       } 
       if(distType=="Binomial"){
         randVec <- rbinom(size,size=as.numeric(input$trials),prob=as.numeric(input$prob))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       } 
       if(distType=="Cauchy"){
         randVec <- rcauchy(size,location=as.numeric(input$location),scale=as.numeric(input$scale))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       } 
       if(distType=="ChiSquare"){
         randVec <- rchisq(size,df=as.numeric(input$df),ncp=as.numeric(input$ncp))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       } 
       if(distType=="Exponential"){
         randVec <- rexp(size,rate=1/as.numeric(input$lambda))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       } 
       if(distType=="F"){
         randVec <- rf(size,df1=as.numeric(input$df1),df2=as.numeric(input$df2),ncp=as.numeric(input$ncp))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       } 
       if(distType=="Gamma"){
         randVec <- rgamma(size,shape=as.numeric(input$shape),scale=as.numeric(input$scale))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Geometric"){
         randVec <- rgeom(size,prob=as.numeric(input$prob))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Hypergeometric"){
         randVec <- rhyper(size,m=as.numeric(input$m),n=as.numeric(input$n),k=as.numeric(input$k))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Log-Normal"){
         randVec <- rlnorm(size,meanlog=as.numeric(input$meanlog),sdlog=as.numeric(input$sdlog))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Negative Binomial"){
         randVec <- rnbinom(size,size=as.numeric(input$size),prob=as.numeric(input$prob))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Normal"){
         randVec <- rnorm(size,mean=as.numeric(input$mean),sd=as.numeric(input$sd))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Poisson"){
         randVec <- rpois(size,lambda=as.numeric(input$lambda))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="t"){
         randVec <- rt(size,df=as.numeric(input$df),ncp=as.numeric(input$ncp))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Uniform"){
         randVec <- runif(size,min=as.numeric(input$min),max=as.numeric(input$max))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
       if(distType=="Weibull"){
         randVec <- rweibull(size,shape=as.numeric(input$shape),scale=as.numeric(input$scale))
         hist(randVec,col="gray",main=paste(distType,"Distribution"),xlab="Random Values")
       }
     })
   }
)
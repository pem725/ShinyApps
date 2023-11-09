### scratch pad
library(tidyverse)
library(lme4)
dat <- read.csv("AllTrustShiny.csv")

dat3 <- subset(dat,study==3)
d3.t <- table(dat3$id,dat3$scen)
d3.t
summary(d3.t)

log10(10)
log(10)
log10(10)
exp(log(4))
exp(log10(10))
exp(4)
exp(1)
exp(1)
?log
log(3, base=2)
log(-5)

m1 <- glm(B ~ T, family= data=dat)
summary(m1)
exp(coef(m1))

plot(dat$T,predict(m1,type = "response"))


dat$gu1r <- dat$G*dat$U1*dat$R

aggregate(dat$gu1r, by=list(dat$scen), mean)




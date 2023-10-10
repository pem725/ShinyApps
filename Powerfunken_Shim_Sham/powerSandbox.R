# sandbox for power stuff

## t-test sim

# https://cran.r-project.org/web/packages/paramtest/vignettes/Simulating-Power.html

## estimate point est using pwr package:
pwr.t.test(n=50, d=.5, type='two.sample')

library('paramtest')
library('pwr')
library('ggplot2')
library('knitr')
library('nlme')
library('lavaan')
library('dplyr')
### now do the sim stuff
# create user-defined function to generate and analyze data
t_func <- function(simNum, N, d) {
  x1 <- rnorm(N, 0, 1)
  x2 <- rnorm(N, d, 1)
  
  t <- t.test(x1, x2, var.equal=TRUE)  # run t-test on generated data
  stat <- t$statistic
  p <- t$p.value
  
  return(c(t=stat, p=p, sig=(p < .05)))
  # return a named vector with the results we want to keep
}

power_ttest <- run_test(t_func, n.iter=5000, output='data.frame', N=50, d=.5)  # simulate data
results(power_ttest) %>%
  summarise(power=mean(sig))

power_ttest_vary <- grid_search(t_func, params=list(N=c(25, 50, 100)),
                                n.iter=5000, output='data.frame', d=.5)
results(power_ttest_vary) %>%
  group_by(N.test) %>%
  summarise(power=mean(sig))

# varying N and Cohen's d
power_ttest_vary2 <- grid_search(t_func, params=list(N=c(25, 50, 100), d=c(.2, .5)),
                                 n.iter=5000, output='data.frame')
power <- results(power_ttest_vary2) %>%
  group_by(N.test, d.test) %>%
  summarise(power=mean(sig))
print(power)
ggplot(power, aes(x=N.test, y=power, group=factor(d.test), colour=factor(d.test))) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's d") +
  theme_minimal()


### linear model code (again, from paramtest vignette page)

lm_test <- function(simNum, N, b1, b0=0, xm=0, xsd=1) {
  x <- rnorm(N, xm, xsd)
  y <- rnorm(N, b0 + b1*x, sqrt(1 - b1^2))  # var. approx. 1 after accounting
  # for explained variance by x
  model <- lm(y ~ x)
  
  # pull output from model
  est <- coef(summary(model))['x', 'Estimate']
  se <- coef(summary(model))['x', 'Std. Error']
  p <- coef(summary(model))['x', 'Pr(>|t|)']
  
  return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), est=est, se=se, p=p,
           sig=(p < .05)))
}

# we vary N at 200 and 300; we are also setting coefficient of x predicting
# y to be approx. .15 across all simulations
power_lm <- grid_search(lm_test, params=list(N=c(200, 300)), n.iter=5000, output='data.frame', b1=.15,
                        parallel='snow', ncpus=4)
results(power_lm) %>%
  group_by(N.test) %>%
  summarise(power=mean(sig))

lm_test_interaction <- function(simNum, N, b1, b2, b3, b0=0, x1m=0, x1sd=1,
                                x2m=0, x2sd=1) {
  
  x1 <- rnorm(N, x1m, x1sd)
  x2 <- rnorm(N, x2m, x2sd)
  yvar <- sqrt(1 - b1^2 - b2^2 - b3^2)  # residual variance
  y <- rnorm(N, b0 + b1*x1 + b2*x2 + b3*x1*x2, yvar)
  model <- lm(y ~ x1 * x2)
  
  # pull output from model (two main effects and interaction)
  est_x1 <- coef(summary(model))['x1', 'Estimate']
  p_x1 <- coef(summary(model))['x1', 'Pr(>|t|)']
  sig_x1 <- p_x1 < .05
  est_x2 <- coef(summary(model))['x2', 'Estimate']
  p_x2 <- coef(summary(model))['x2', 'Pr(>|t|)']
  sig_x2 <- p_x2 < .05
  est_int <- coef(summary(model))['x1:x2', 'Estimate']
  p_int <- coef(summary(model))['x1:x2', 'Pr(>|t|)']
  sig_int <- p_int < .05
  
  return(c(est_x1=est_x1, p_x1=p_x1, sig_x1=sig_x1, est_x2=est_x2, p_x2=p_x2,
           sig_x2=sig_x2, est_int=est_int, p_int=p_int, sig_int=sig_int))
}

# varying N at 200 and 300; setting coefficient of x1 = .15, coefficient of
# x2 = 0, and coefficien of interaction = .3
power_lm_int <- grid_search(lm_test_interaction, params=list(N=c(200, 300)),
                            n.iter=5000, output='data.frame', b1=.15, b2=0, b3=.3, parallel='snow', ncpus=4)
results(power_lm_int) %>%
  group_by(N.test) %>%
  summarise(
    power_x1=mean(sig_x1),
    power_x2=mean(sig_x2),
    power_int=mean(sig_int))

lm_test_simple <- function(simNum, N, b1, b2, b3, b0=0, x1m=0, x1sd=1, x2m=0, x2sd=1) {
  x1 <- rnorm(N, x1m, x1sd)
  x2 <- rnorm(N, x2m, x2sd)
  yvar <- sqrt(1 - b1^2 - b2^2 - b3^2)
  y <- rnorm(N, b0 + b1*x1 + b2*x2 + b3*x1*x2, yvar)
  model <- lm(y ~ x1 * x2)  # here is the original model
  
  est_int <- coef(summary(model))['x1:x2', 'Estimate']
  p_int <- coef(summary(model))['x1:x2', 'Pr(>|t|)']
  sig_int <- p_int < .05
  
  # calculate x1 at +/- 1 SD, to look at simple effects
  x1minus1sd <- x1 - mean(x1) + sd(x1)
  x1plus1sd <- x1 - mean(x1) - sd(x1)
  
  # new models to examine simple effects
  model2 <- lm(y ~ x1minus1sd * x2)
  model3 <- lm(y ~ x1plus1sd * x2)
  
  # test effect of x2 when x1 is at +/- 1 SD
  est_x2_minus1 <- coef(summary(model2))['x2', 'Estimate']
  p_x2_minus1 <- coef(summary(model2))['x2', 'Pr(>|t|)']
  sig_x2_minus1 <- p_x2_minus1 < .05
  
  est_x2_plus1 <- coef(summary(model3))['x2', 'Estimate']
  p_x2_plus1 <- coef(summary(model3))['x2', 'Pr(>|t|)']
  sig_x2_plus1 <- p_x2_plus1 < .05
  
  return(c(est_int=est_int, p_int=p_int, sig_int=sig_int,
           est_x2_minus1=est_x2_minus1, p_x2_minus1=p_x2_minus1,
           sig_x2_minus1=sig_x2_minus1, est_x2_plus1=est_x2_plus1,
           p_x2_plus1=p_x2_plus1, sig_x2_plus1=sig_x2_plus1))
}

power_lm_simple <- grid_search(lm_test_simple, params=list(N=c(200, 300)),
                               n.iter=5000, output='data.frame', b1=.15, b2=0, b3=.3, parallel='snow', ncpus=4)
results(power_lm_simple) %>%
  group_by(N.test) %>%
  summarise(
    power_x2_minus1=mean(sig_x2_minus1),
    power_x2_plus1=mean(sig_x2_plus1))


### multilevel models

mlm_test <- function(simNum, N, b1, b0=0, xm=0, xsd=1, varInt=1, varSlope=1, varResid=1) {
  timePoints <- 4
  subject <- rep(1:N, each=timePoints)
  sub_int <- rep(rnorm(N, 0, sqrt(varInt)), each=timePoints)  # random intercept
  sub_slope <- rep(rnorm(N, 0, sqrt(varSlope)), each=timePoints)  # random slope
  time <- rep(0:(timePoints-1), N)
  y <- (b0 + sub_int) + (b1 + sub_slope)*time + rnorm(N*timePoints, 0, sqrt(varResid))
  # y-intercept as a function of b0 plus random intercept;
  # slope as a function of b1 plus random slope
  data <- data.frame(subject, sub_int, sub_slope, time, y)
  
  # for more complex models that might not converge, tryCatch() is probably
  # a good idea
  return <- tryCatch({
    model <- nlme::lme(y ~ time, random=~time|subject, data=data)
    # when using parallel processing, we must refer to functions from
    # packages directly, e.g., package::function()
    
    est <- summary(model)$tTable['time', 'Value']
    se <- summary(model)$tTable['time', 'Std.Error']
    p <- summary(model)$tTable['time', 'p-value']
    return(c(est=est, se=se, p=p, sig=(p < .05)))
  },
  error=function(e) {
    #message(e)  # print error message
    return(c(est=NA, se=NA, p=NA, sig=NA))
  })
  
  return(return)
}

# I am cutting this down to 500 iterations so that the document compiles faster; I would, however,
# recommend more iterations for a stable estimate
power_mlm <- grid_search(mlm_test, params=list(N=c(200, 300)), n.iter=500, output='data.frame', b1=.15,
                         varInt=.05, varSlope=.15, varResid=.4, parallel='snow', ncpus=4)
results(power_mlm) %>%
  group_by(N.test) %>%
  summarise(
    power=mean(sig, na.rm=TRUE),
    na=sum(is.na(sig)))  # we use this to count up how many cases did not properly converge


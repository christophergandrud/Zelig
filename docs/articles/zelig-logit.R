## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
        stop_on_error = 2L
)

## ----eval=FALSE----------------------------------------------------------
#  z5 <- zlogit$new()
#  z5$zelig(Y ~ X1 + X ~ X, weights = w, data = mydata)
#  z5$setx()
#  z5$sim()

## ----eval=FALSE----------------------------------------------------------
#  z.out <- zelig(Y ~ X1 + X2, model = "logit", weights = w, data = mydata)
#  x.out <- setx(z.out)
#  s.out <- sim(z.out, x = x.out, x1 = NULL)

## ------------------------------------------------------------------------
library(Zelig)
data(turnout)

## ------------------------------------------------------------------------
z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout, cite = FALSE)

## ------------------------------------------------------------------------
summary(z.out1)

## ------------------------------------------------------------------------
x.out1 <- setx(z.out1, age = 36, race = "white")

## ------------------------------------------------------------------------
s.out1 <- sim(z.out1, x = x.out1)
summary(s.out1)

## ----fig.height=11, fig.width=7------------------------------------------
plot(s.out1)

## ------------------------------------------------------------------------
z.out2 <- zelig(vote ~ race + educate, model = "logit", data = turnout, 
                cite = FALSE)
x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))
s.out2 <- sim(z.out2, x = x.high, x1 = x.low)
summary(s.out2)

## ----fig.height=11, fig.width=7------------------------------------------
plot(s.out2)


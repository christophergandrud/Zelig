## ----setup, include=FALSE------------------------------------------------
knitr::opts_knit$set(
        stop_on_error = 2L
)

## ---- message=FALSE------------------------------------------------------
library(Zelig)
library(dplyr)

z.log <- zelig(speed ~ log(dist), data = cars, model = 'ls', cite = FALSE)

## ------------------------------------------------------------------------
setx(z.log, dist = log(26:56)) %>%
    sim() %>%
    plot(xlab = 'dist (log)') 

## ------------------------------------------------------------------------
cars.poly <- cbind(cars, I(cars$dist^2))
names(cars.poly)[ncol(cars.poly)] <- 'dist_2'

head(cars.poly)

## ---- message=FALSE------------------------------------------------------
dist_range <- min(cars.poly$dist):max(cars.poly$dist)
zelig(speed ~ dist + dist_2, data = cars.poly, model = 'ls', cite = FALSE) %>%
    setx(dist = dist_range, dist_2 = dist_range^2) %>%
    sim() %>%
    plot()

## ---- message=FALSE------------------------------------------------------
library(splines)
cars.splines <- cbind(cars, bs(cars$dist))
names(cars.splines)[3:5] <- c('dist_bs_1', 'dist_bs_2', 'dist_bs_3')

zelig(speed ~ dist_bs_1 + dist_bs_2 + dist_bs_3, data = cars.splines, 
      model = 'ls', cite = FALSE) %>%
    setx(dist_bs_1 = c(min(cars.splines$dist_bs_1), median(cars.splines$dist_bs_1), 
                       max(cars.splines$dist_bs_2)),
         dist_bs_2 = c(min(cars.splines$dist_bs_2), median(cars.splines$dist_bs_2), 
                       max(cars.splines$dist_bs_2)),
         dist_bs_3 = c(min(cars.splines$dist_bs_3), median(cars.splines$dist_bs_3), 
                       max(cars.splines$dist_bs_3))) %>%
    sim() %>%
    plot()


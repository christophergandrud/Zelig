# REQUIRE TEST arima Monte Carlo -----------------------------------------------

## Need to implement ##

# REQUIRE TEST arima successful estimation -------------------------------------
test_that('REQUIRE TEST arima successful estimation', {
    data(seatshare)
    ts <- zarima$new()

    ## NEEDS a better test, possibly once get_coef has been implemented for arima
    expect_error(
    ts$zelig(unemp ~ leftseat, order = c(1,0,1), ts = "year", cs = "country",
              data = seatshare),
              NA)
})

# FAIL TEST arima fails if DV does not vary ------------------------------------
test_that('FAIL TEST arima fails if DV does not vary', {
    no_vary_df <- data.frame(country = c(rep("A", 5), rep("B", 5)),
                             year = c(1:5, 1:5),
                             y = c(rep(1:5), rep(2, 5)),
                         x = c(1, 3, -1, NA, 1, NA, 1, 2, NA, 5))
   # a.out <- amelia(x = no_vary_df, cs = "country", ts = "year")

    zts <- zarima$new()
    expect_error(
        zts$zelig(y ~ x, ts = 'year', cs = 'country', order = c(1, 0, 1),
            data = no_vary_df),
            'Dependent variable does not vary for at least one of the cases.')
})


# FAIL TEST arima models ------------------------------------
test_that('REQUIRE TEST arima models', {

    n.obs <- 2000
    x <- rnorm(n=n.obs)
    z <- rnorm(n=n.obs)
    t <- 1:n.obs
    r <- rep(c(1,2),n.obs/2)
    beta <- 1
    phi <-0.3

    y <- rep(NA,n.obs)
    y[1]<-beta*x[1] + rnorm(1)
    for(i in 2:n.obs){
        y[i] <- phi*y[i-1] + beta*x[i] + rnorm(n=1, mean=0, sd=0.2)
    }

    mydata <- data.frame(y,x,z,t,r)
    mydata2 <- rbind(mydata[10:n.obs,],mydata[1:9,])    # reorder dataset

    # check ar model
    zj <- zar$new()
    zj$zelig(y~x + z , data=mydata, ts="t")
    expect_equivalent(length(zj$get_coef()[[1]]), 4)

    # check ma model
    zj <- zma$new()
    zj$zelig(y~x + z , data=mydata, ts="t")
    expect_equivalent(length(zj$get_coef()[[1]]), 4)

    # check ar-2, ma-1 model
    zj <- zarima$new()
    zj$zelig(y~x + z , order=c(2,0,1), data=mydata, ts="t")
    expect_equivalent(length(zj$get_coef()[[1]]), 6)

    # check integration
    zj <- zarima$new()
    zj$zelig(y~x + z , order=c(2,1,1), data=mydata, ts="t")
    expect_equivalent(length(zj$get_coef()[[1]]), 5)

    # check obervations out of time order
    zj <- zarima$new()
    zj$zelig(y~x + z -1, order=c(2,0,1), data=mydata2, ts="t")
    expect_equivalent(length(zj$get_coef()[[1]]), 5)

    zj$setx()
    zj$setx1(x=2)
    zj$sim()


    # ACF plot

    myorder <- eval(zj$zelig.call$order)
    mycoef <- coef(zj$zelig.out$z.out[[1]])
    myparams <- zj$simparam$simparam[[1]]

    test <- Zelig:::simacf(coef=mycoef, order=myorder, params=myparams, alpha = 0.5)

    expect_true(is.null(zeligACFplot(test, omitzero=TRUE)))

    # plots

    expect_true(is.null(ci.plot(zj, qi="pvseries.shock")))
    expect_true(is.null(ci.plot(zj, qi="pvseries.innovation")))
    expect_true(is.null(plot(zj)))

})

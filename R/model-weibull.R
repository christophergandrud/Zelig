#' Weibull Regression for Duration Dependent Variables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig_weibull.html}
#' @import methods
#' @export Zelig-tobit-bayes
#' @exportClass Zelig-tobit-bayes
#'
#' @include model-zelig.R

zweibull <- setRefClass("Zelig-weibull",
                        contains = "Zelig",
                        fields = list(simalpha = "list",
                                      linkinv = "function",
                                      lambda = "ANY"))

zweibull$methods(
  initialize = function() {
    callSuper()
    .self$name <- "weibull"
    .self$authors <- "Olivia Lau, Kosuke Imai, Gary King"
    .self$packageauthors <- "Terry M Therneau, and Thomas Lumley"
    .self$year <- 2007
    .self$description <- "Weibull Regression for Duration Dependent Variables"
    .self$fn <- quote(survival::survreg)
    .self$linkinv <- survreg.distributions[["weibull"]]$itrans
    # JSON
    .self$outcome <- "bounded"
    .self$wrapper <- "weibull"
    .self$acceptweights <- TRUE
  }
)

zweibull$methods(
  zelig = function(formula, ..., robust = FALSE, cluster = NULL, data, weights = NULL, by = NULL, bootstrap = FALSE) {

    localFormula <- formula # avoids CRAN warning about deep assignment from formula existing separately as argument and field
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    if (!(is.null(cluster) || robust))
      stop("If cluster is specified, then `robust` must be TRUE")
    # Add cluster term
    if (robust || !is.null(cluster))
      localFormula <- cluster.formula(localFormula, cluster)
    .self$model.call$dist <- "weibull"
    .self$model.call$model <- FALSE
    callSuper(formula = localFormula, data = data, ..., robust = robust,
              cluster = cluster,  weights = weights, by = by, bootstrap = bootstrap)

    if(!robust){
      fn2 <- function(fc, data) {
        fc$data <- data
        return(fc)
      }
      robust.model.call <- .self$model.call
      robust.model.call$robust <- TRUE

      robust.zelig.out <- .self$data %>%
      group_by_(.self$by) %>%
      do(z.out = eval(fn2(robust.model.call, quote(as.data.frame(.))))$var )

      .self$test.statistics<- list(robust.se = robust.zelig.out$z.out)
    }
  }
)

zweibull$methods(
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      coeff <- coef(z.out)
      mu <- c(coeff, log(z.out$scale) )  # JH this is the scale of the vcov used below
      cov <- vcov(z.out)
      simulations <- mvrnorm(.self$num, mu = mu, Sigma = cov)
      simparam.local <- as.matrix(simulations[, 1:length(coeff)])
      simalpha.local <- as.matrix(simulations[, (length(coeff)+1)])
      simparam.local <- list(simparam = simparam.local, simalpha = simalpha.local)
      return(simparam.local)
    } else if(identical(method,"point")){
      return(list(simparam = t(as.matrix(coef(z.out))), simalpha = log(z.out$scale)))
    }
  }
)

zweibull$methods(
  qi = function(simparam, mm) {
    eta <- simparam$simparam %*% t(mm)
    theta <- as.matrix(apply(eta, 2, linkinv))
    ev <- theta * gamma(1 + exp(simparam$simalpha))
    pv <- as.matrix(rweibull(length(ev), shape = 1/exp(simparam$simalpha), scale = theta))
    return(list(ev = ev, pv = pv))
  }
)

zweibull$methods(
  mcfun = function(x, b0=0, b1=1, alpha=1, sim=TRUE){
    .self$mcformula <- as.Formula("Surv(y.sim, event) ~ x.sim")


    mylambda <-exp(b0 + b1 * x)
    event <- rep(1, length(x))
    y.sim <- rweibull(n=length(x), shape=alpha, scale=mylambda)
    y.hat <- mylambda * gamma(1 + (1/alpha))

    if(sim){
        mydata <- data.frame(y.sim=y.sim, event=event, x.sim=x)
        return(mydata)
    }else{
        mydata <- data.frame(y.hat=y.hat, event=event, x.seq=x)
        return(mydata)
    }
  }
)

#' Logistic Regression for Dichotomous Dependent Variables
#'
#' @param formula an object of class "formula" (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object coercible
#'   by `as.data.frame` to a data frame) containing the variables in the model.
#'   If not found in data, the variables are taken from `environment(formula)`,
#'   typically the environment from which `lm`` is called.
#'
#'   Imputed data sets created with \link{amelia} or \code{\link{to_zelig_mi}}
#'   can also be supplied.
#' @param ... arguments to pass to \code{\link{glm}}.
#' @param model a character string specifying the model type. NOTE: only
#'   required if using the Zelig 4 wrappers.
#' @param weights an optional vector of weight values or character string
#'   identifying a weighting variable in `data`. Weights adjust the observed
#'   sample distribution in the data to an underlying population of interest.
#'
#'   - If the supplied weights are all integer values, then `zelig` rebuilds a new
#'   version of the dataset by duplicating observations according to their
#'   weight (and removing observations with zero weight).
#'
#'   - If the weights are continuously valued, `zelig`` bootstraps the supplied
#'   dataset, using the relative weights as bootstrap probabilities.
#'
#' @param by an optional character string identifying a variable in `data`
#'   to conduct the model analysis along. This can be used to run separate
#'   models for different levels of a categorical variable.
#' @param bootstrap logical whether or not to bootstrap the data set.
#' @param cite logical whether or not to return model citation information to
#'   the console when the model has been estimated.
#'
#' @details
#' Logistic regression specifies a dichotomous dependent variable as a function
#' of a set of explanatory variables.
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zelig_logit.html}
#'
#' @examples
#' \dontrun{
#' # Workflow: Zelig 5 Reference Classes
#' z5 <- zlogit$new()
#' z5$zelig(Y ~ X1 + X ~ X, weights = w, data = mydata)
#' z5$setx()
#' z5$sim()
#'
#' # Workflow: Zelig 4 Wrappers
#' z.out <- zelig(Y ~ X1 + X2, model = "logit", weights = w, data = mydata)
#' x.out <- setx(z.out)
#' s.out <- sim(z.out, x = x.out, x1 = NULL)
#' }
#'
#' # Basic example with Zelig 4 wrappers
#' data(turnout)
#'
#' # Estimate model
#' z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout,
#'                 cite = FALSE)
#'
#' # Set fitted values for a 36 year old white subject
#' x.out1 <- setx(z.out1, age = 36, race = "white")
#'
#' # Simulate
#' s.out1 <- sim(z.out1, x = x.out1)
#'
#' \dontrun{
#' # Plot quantities of interest
#' plot(s.out1)
#' }
#'
#' @import methods
#' @export Zelig-logit
#' @exportClass Zelig-logit
#'
#' @include model-zelig.R
#' @include model-gee.R
#' @include model-gamma.R
#' @include model-zelig.R
#' @include model-glm.R
#' @include model-binchoice.R

zlogit <- setRefClass("Zelig-logit",
                      contains = "Zelig-binchoice")

zlogit$methods(initialize = function() {
    callSuper()
    .self$name <- "logit"
    .self$link <- "logit"
    .self$description = "Logistic Regression for Dichotomous Dependent Variables"
    .self$packageauthors <- "R Core Team"
    .self$wrapper <- "logit"
})

zlogit$methods(mcfun = function(x, b0 = 0, b1 = 1, ..., sim = TRUE) {
    mu <- 1/(1 + exp(-b0 - b1 * x))
    if (sim) {
        y <- rbinom(n = length(x), size = 1, prob = mu)
        return(y)
    } else {
        return(mu)
    }
})

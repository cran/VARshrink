#' Summary method for an object of class 'varshrinkest',
#' VAR parameters estimated by VARshrink()
#'
#' Extend summary.varest() to class 'varshrinest' to incorporate
#' adapted methods for new classes:
#' summary.shrinklm(), logLik.varshrinkest(), roots.varshrinkest().
#'
#' Code is modified to avoid call to data matrices ($y, $datamat)
#' and to use effective numbers of parameters of shrinkage estimates.
#'
#' Output includes the scale matrix, Sigma, and degree-of-freedom, dof,
#' for multivariate t-distribution for residuals.
#'
# Last modified: 2019.7.30. Namgil Lee @ Kangwon National University
#' @param object An object of class "varshrinkest", usually
#' a result of call to "VARshrink()".
#' @param equations Subset of names of endogenous time series variables
#' to summarize.
#' @param ... Currently not used.
#' @importFrom stats resid cov df.residual cor
#' @examples
#' data(Canada, package = "vars")
#' y <- diff(Canada)
#' estim <- VARshrink(y, p = 2, type = "const", method = "ridge")
#' summary(estim)
#' @export
summary.varshrinkest <- function (object, equations = NULL, ...) {
  ynames <- names(object$varresult)
  obs <- object$obs
  if (is.null(equations)) {
    ysubnames <- ynames
  }
  else {
    ysubnames <- as.character(equations)
    if (!(all(ysubnames %in% ynames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      ysubnames <- ynames[1]
    }
  }
  eqest <- lapply(object$varresult[ysubnames], summary)
  resids <- resid(object)
  Sigma <- if (is.null(object$Sigma)) {
    crossprod(resids) / obs
  } else {
    object$Sigma
  }
  dof <- ifelse(is.null(object$dof), Inf, object$dof)
  covres <- cov(resids) * (obs - 1) / min(sapply(object$varresult, df.residual))
  corres <- cor(resids)
  logLik <- as.numeric(logLik(object))
  roots <- roots_sh(object)
  result <- list(names = ysubnames, varresult = eqest, covres = covres,
                 corres = corres, logLik = logLik, obs = obs, roots = roots,
                 type = object$type, call = object$call,
                 Sigma = Sigma, dof = dof)
  class(result) <- c("varshsum", "varsum")
  return(result)
}

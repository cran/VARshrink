## ----setup0, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, warning=FALSE----------------------------------------
text_tbl <- data.frame(
FM = c("VAR",                  "fevd",    "irf",              "predict", "summary",        "arch.test_sh", "normality.test_sh", "serial.test_sh", "stability_sh"), 
CL = c("varshrinkest, varest", "varfevd", "varshirf, varirf", "varprd",  "varshsum, varsum",    "varcheck",      "varcheck",          "varcheck",       "varstabil"),
MC = c("coef, fevd, fitted, irf, logLik, Phi, plot, predict, print, Psi, resid, summary",                              "plot, print", "plot, print",  "plot, print", "print",                "plot, print",   "plot, print",       "plot, print",    "plot, print"), 
FC = c("Acoef_sh, arch.test_sh, Bcoef_sh, BQ_sh, causality_sh, normality.test_sh, restrict_sh, roots_sh, serial.test_sh, stability_sh", " ", " ", "fanchart", " ", " ", " ", " ", " ")
)
colnames(text_tbl) <- c("Function or method", "Class", "Methods for class", "Functions for class")
kableExtra::column_spec(
  knitr::kable(text_tbl,
       caption = "Table 1. Structure of the package VARshrink."),
  1:4, width = "7em", border_left = FALSE, border_right = FALSE)

## ----setup, include = FALSE----------------------------------------------
library(VARshrink)

## ---- results = "hide", message=FALSE------------------------------------
set.seed(1000)
myCoef <- list(A = list(matrix(c(0.5, 0, 0, 0.5), 2, 2)), c = c(0.2, 0.7))
myModel <- list(Coef = myCoef, Sigma = diag(0.1^2, 2), dof = Inf)
Y <- simVARmodel(numT = 100, model = myModel, burnin = 10)
resu_estim <- list()

## ------------------------------------------------------------------------
resu_estim$`Ridge regression` <-
  VARshrink(Y, p = 1, type = "const", method = "ridge", lambda = NULL)
resu_estim$`Ridge regression`

## ------------------------------------------------------------------------
summary(resu_estim$`Ridge regression`)

## ------------------------------------------------------------------------
resu_estim$`Nonparametric shrinkage` <-
  VARshrink(Y, p = 1, type = "const", method = "ns",
                    lambda = NULL, lambda_var = NULL)
resu_estim$`Nonparametric shrinkage`

## ------------------------------------------------------------------------
summary(resu_estim$`Nonparametric shrinkage`)

## ------------------------------------------------------------------------
resu_estim$`Full Bayes (fixed dof)` <-
  VARshrink(Y, p = 1, type = "const", method = "fbayes", dof = 6,
                     burnincycle = 1000, mcmccycle = 2000)
resu_estim$`Full Bayes (fixed dof)`

## ------------------------------------------------------------------------
summary(resu_estim$`Full Bayes (fixed dof)`)

## ------------------------------------------------------------------------
resu_estim$`Full Bayes (estim dof)` <-
  VARshrink(Y, p = 1, type = "const", method = "fbayes", dof = NULL,
            burnincycle = 1000, mcmccycle = 2000)
resu_estim$`Full Bayes (estim dof)`

## ------------------------------------------------------------------------
resu_estim$`Semi Bayes (fixed dof)` <-
  VARshrink(Y, p = 1, type = "const", method = "sbayes", dof = 6,
            lambda = NULL, lambda_var = NULL, prior_type = "NCJ",
            num_folds = 5, m0 = ncol(Y))
resu_estim$`Semi Bayes (fixed dof)`
summary(resu_estim$`Semi Bayes (fixed dof)`)

## ------------------------------------------------------------------------
resu_estim$`Semi Bayes (estim dof)` <-
  VARshrink(Y, p = 1, type = "const", method = "sbayes", dof = NULL,
            lambda = NULL, lambda_var = NULL, prior_type = "NCJ",
            num_folds = 5, m0 = ncol(Y))
resu_estim$`Semi Bayes (estim dof)`

## ------------------------------------------------------------------------
resu_estim$`K-fold CV (fixed dof)` <-
  VARshrink(Y, p = 1, type = "const", method = "kcv", dof = 6,
            lambda = NULL, lambda_var = NULL, prior_type = "NCJ",
            num_folds = 5, m0 = ncol(Y))
resu_estim$`K-fold CV (fixed dof)`

## ---- results = "hide"---------------------------------------------------
resu_estim$`K-fold CV (estim dof)` <-
  VARshrink(Y, p = 1, type = "const", method = "kcv", dof = NULL,
            lambda = NULL, lambda_var = NULL, prior_type = "NCJ",
            num_folds = 5, m0 = ncol(Y))

## ------------------------------------------------------------------------
resu_sse <- data.frame(SSE = sapply(resu_estim,
  function(x) calcSSE_Acoef(Acoef_sh(x), myCoef$A)))

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(round(resu_sse, 3),
             caption = "Table 2. Sum of squared errors of VAR coefficients estimated by the shrinkage methods.")

## ----diffCanada, out.width='70%', fig.cap = "Figure 1. The benchmark data set obtained by differencing the Canada data."----
data(Canada, package = "vars")
Y = diff(Canada)
plot(Y, cex.lab = 1.3)

## ----modelcomp, echo = FALSE---------------------------------------------
load("table3_modelcomp.RData")
knitr::kable(round(resu_model, 1),
             caption = "Table 3. Information criteria (AIC, BIC) for model comparison.")

## ----pred, fig.cap="Figure 2. A 10-step ahead Forecasting of time series by the VAR model estimated by the nonparametric shrinkage method. The differenced Canada data were modeled by a VAR(2) model selected at the minimum BIC."----
plot(predict(VARshrink(Y, p = 2, type = "const", method = "ns")), names = "U")


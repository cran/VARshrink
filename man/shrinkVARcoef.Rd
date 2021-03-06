% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shrinkVARcoef.R
\name{shrinkVARcoef}
\alias{shrinkVARcoef}
\title{Semiparametric Bayesian Shrinkage Estimator for
Multivariate Regression}
\usage{
shrinkVARcoef(Y, X, lambda, dof = Inf, prior_type = "NCJ",
  TolDRes = 1e-04, m0 = ncol(Y))
}
\arguments{
\item{Y}{An N x K matrix of dependent variables.}

\item{X}{An N x M matrix of regressors.}

\item{lambda}{A shrinkage intensity parameter value between 0~1.}

\item{dof}{Degree of freedom for multivariate t-distribution.
If NULL or Inf, then use multivariate normal distribution.}

\item{prior_type}{"NCJ" for non-conjugate prior and "CJ" for conjugate
prior for scale matrix Sigma.}

\item{TolDRes}{Tolerance parameter for stopping criterion.}

\item{m0}{A hyperparameter for inverse Wishart distribution for Sigma}
}
\description{
Compute the semiparametric Bayesian shrinkage estimator of Psi and Sigma
for a given shrinkage parameter lambda.
The function is a private function for lm_semi_Bayes_PCV() and
lm_ShVAR_KCV().
}
\references{
N. Lee, H. Choi, and S.-H. Kim (2016). Bayes shrinkage
estimation for high-dimensional VAR models with scale mixture of normal
distributions for noise. Computational Statistics & Data Analysis 101,
250-276. doi: 10.1016/j.csda.2016.03.007
}

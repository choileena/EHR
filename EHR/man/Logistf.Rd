\name{Logistf}
\alias{Logistf}
\title{
  Firth's penalized-likelihood logistic regression with more decimal places of 
  p-value than \code{\link{logistf}} function in the R package \pkg{logistf}
}
\description{
  Adapted from \code{\link{logistf}} in the R package \pkg{logistf}, this is 
  the same as \code{\link{logistf}} except that it provides more decimal places 
  of p-value that would be useful for Genome-Wide Association Study (GWAS) 
  or Phenome Wide Association Study (PheWAS).
}
\usage{
    Logistf(formula = attr(data, "formula"), data = sys.parent(), pl = TRUE, 
    alpha = 0.05, control, plcontrol, firth = TRUE, init, weights, 
    plconf = NULL, dataout = TRUE, ...)
}
\arguments{
  \item{formula}{
  a formula object, with the response on the left of the  operator, and the 
  model terms on the right. The response must be a vector with 0 and 1 or 
  FALSE and TRUE for the  outcome, where the higher value (1 or TRUE) is 
  modeled. It is possible to include contrasts, interactions, nested effects, 
  cubic or polynomial splines and all S features as well, e.g. \code{Y ~}
  \code{X1*X2 + ns(X3, df=4)}. From version 1.10, you may also include offset() 
  terms.
    }
  \item{data}{
  a data.frame where the variables named in the formula can be found, i. e. 
  the variables containing the binary response and the covariates.
    }
  \item{pl}{
  specifies if confidence intervals and tests should be based on the profile
  penalized log likelihood (\code{pl=TRUE}, the default) or on the Wald method
  (\code{pl=FALSE}).
    }
  \item{alpha}{
  the significance level (1-\eqn{\alpha} the confidence level, 
  0.05 as default).}
  \item{control}{
  Controls Newton-Raphson iteration. Default is \code{control=}
  \code{logistf.control(maxstep,}
  \code{maxit, maxhs, lconv, gconv, xconv})
}
  \item{plcontrol}{
  Controls Newton-Raphson iteration for the estimation of the profile likelihood
  confidence intervals. Default is \code{plcontrol=logistpl.control(maxstep,}
  \code{maxit,}  \code{maxhs, lconv, xconv, ortho, pr})
}
  \item{firth}{
   use of Firth's penalized maximum likelihood (\code{firth=TRUE}, default) or
   the standard maximum likelihood method (\code{firth=FALSE}) for the logistic
   regression. Note that by specifying \code{pl=TRUE} and \code{firth=FALSE} 
   (and probably a lower number of iterations) one obtains profile likelihood 
   confidence intervals for maximum likelihood logistic regression parameters.
}
  \item{init}{
  specifies the initial values of the coefficients for the fitting algorithm.
}
  \item{weights}{
  specifies case weights. Each line of the input data set is multiplied by the
  corresponding element of \code{weights}.
}
  \item{plconf}{
  specifies the variables (as vector of their indices) for which profile likelihood
  confidence intervals should be computed. Default is to compute for all variables.
}
  \item{dataout}{
  If TRUE, copies the \code{data} set to the output object.
}
  \item{\dots}{
  Further arguments to be passed to logistf.
}
}

\value{
  same as \code{\link{logistf}} except for providing more decimal places 
  of p-value.
}

\author{
  Leena Choi <naturechoi@gmail.com> and Cole Beck <cole.beck@Vanderbilt.Edu>
}

\references{
  same as those provided in the R package \pkg{logistf}.
}

\examples{
data(dataPheWAS)
fit <- Logistf(X264.3 ~ exposure + age + race + gender, data=dd)
summary(fit)
}

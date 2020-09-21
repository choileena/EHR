#' Firth's penalized-likelihood logistic regression with more decimal places of
#' p-value than \code{logistf} function in the R package \sQuote{logistf}
#'
#' Adapted from \code{logistf} in the R package \sQuote{logistf}, this is 
#' the same as \code{logistf} except that it provides more decimal places 
#' of p-value that would be useful for Genome-Wide Association Study (GWAS) 
#' or Phenome Wide Association Study (PheWAS).
#'
#' @param formula a formula object, with the response on the left of the
#' operator, and the model terms on the right. The response must be a vector
#' with 0 and 1 or FALSE and TRUE for the  outcome, where the higher value (1 or
#' TRUE) is modeled. It is possible to include contrasts, interactions, nested
#' effects, cubic or polynomial splines and all S features as well, e.g.
#' \code{Y ~ X1*X2 + ns(X3, df=4)}. From version 1.10, you may also include
#' offset() terms.
#' @param data a data.frame where the variables named in the formula can be
#' found, i. e. the variables containing the binary response and the covariates.
#' @param pl specifies if confidence intervals and tests should be based on the
#' profile penalized log likelihood (\code{pl=TRUE}, the default) or on the Wald
#' method (\code{pl=FALSE}).
#' @param alpha the significance level (1-\eqn{\alpha} the confidence level,
#' 0.05 as default).
#' @param control Controls Newton-Raphson iteration. Default is \cr
#' \code{control=logistf.control(maxstep, maxit, maxhs, lconv, gconv, xconv})
#' @param plcontrol Controls Newton-Raphson iteration for the estimation of the
#' profile likelihood confidence intervals. Default is \cr
#' \code{plcontrol=logistpl.control(maxstep, maxit,}
#' \code{maxhs, lconv, xconv, ortho, pr)}
#' @param firth use of Firth's penalized maximum likelihood (\code{firth=TRUE},
#' default) or the standard maximum likelihood method (\code{firth=FALSE}) for
#' the logistic regression. Note that by specifying \code{pl=TRUE} and
#' \code{firth=FALSE} (and probably a lower number of iterations) one obtains
#' profile likelihood confidence intervals for maximum likelihood logistic
#' regression parameters.
#' @param init specifies the initial values of the coefficients for the fitting
#' algorithm.
#' @param weights specifies case weights. Each line of the input data set is
#' multiplied by the corresponding element of \code{weights}.
#' @param plconf specifies the variables (as vector of their indices) for which
#' profile likelihood confidence intervals should be computed. Default is to
#' compute for all variables.
#' @param dataout If TRUE, copies the \code{data} set to the output object.
#' @param \dots Further arguments to be passed to logistf.
#'
#' @return
#' same as \code{logistf} except for providing more decimal places of p-value.
#'
#' @templateVar author choibeck
#' @template auth
#'
#' @references same as those provided in the R package \sQuote{logistf}.
#'
#' @examples
#' data(dataPheWAS)
#' fit <- Logistf(X264.3 ~ exposure + age + race + gender, data=dd)
#' summary(fit)
#' @export

`Logistf` <- 
function (formula = attr(data, "formula"), data = sys.parent(), 
    pl = TRUE, alpha = 0.05, control, plcontrol, firth = TRUE, 
    init, weights, plconf = NULL, dataout = TRUE, ...) 
{
    if(!requireNamespace("logistf", quietly = TRUE)) {
      stop("Logistf requires the logistf package, please install it.",
        call. = FALSE)
    }
    ns <- loadNamespace('logistf')
    logistpl <- getFromNamespace('logistpl', ns)
    logistf.fit <- getFromNamespace('logistf.fit', ns)

    call <- match.call()
    if (missing(control)) 
        control <- logistf::logistf.control()
    if (pl == TRUE & missing(plcontrol)) 
        plcontrol <- logistf::logistpl.control()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "weights", "na.action", "offset"), 
        names(mf), 0L)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    y <- model.response(mf)
    n <- length(y)
    x <- model.matrix(formula, data = data)
    k <- ncol(x)
    cov.name <- labels(x)[[2]]
    weight <- as.vector(model.weights(mf))
    offset <- as.vector(model.offset(mf))
    if (is.null(offset)) 
        offset <- rep(0, n)
    if (is.null(weight)) 
        weight <- rep(1, n)
    if (missing(init)) 
        init <- rep(0, k)
    if (is.null(plconf) & pl == TRUE) 
        plconf <- 1:k
    if (dimnames(x)[[2]][1] == "(Intercept)") {
        int <- 1
        coltotest <- 2:k
    }
    else {
        int <- 0
        coltotest <- 1:k
    }
    fit.full <- logistf.fit(x = x, y = y, weight = weight, offset = offset,
        firth, col.fit = 1:k, init, control = control)
    fit.null <- logistf.fit(x = x, y = y, weight = weight, offset = offset,
        firth, col.fit = int, init, control = control)
    fit <- list(coefficients = fit.full$beta, alpha = alpha,
        terms = colnames(x), var = fit.full$var, df = (k - int),
        loglik = c(fit.null$loglik, fit.full$loglik), iter = fit.full$iter,
        n = sum(weight), y = y, formula = formula(formula), call = match.call(),
        conv = fit.full$conv)
    names(fit$conv) <- c("LL change", "max abs score", "beta change")
    beta <- fit.full$beta
    covs <- fit.full$var
    pi <- fit.full$pi
    fit$firth <- firth
    fit$linear.predictors <- as.vector(x %*% beta + offset)
    fit$predict <- fit.full$pi
    fit$hat.diag <- fit.full$Hdiag
    if (firth) 
        fit$method <- "Penalized ML"
    else fit$method <- "Standard ML"
    vars <- diag(covs)
    fit$prob <- pchisq((beta^2/vars), 1, lower.tail=F)
    fit$method.ci <- rep("Wald", k)
    fit$ci.lower <- as.vector(beta + qnorm(alpha/2) * vars^0.5)
    fit$ci.upper <- as.vector(beta + qnorm(1 - alpha/2) * vars^0.5)
    fit$alpha <- alpha
    fit$conflev <- 1 - alpha
    if (pl) {
        betahist.lo <- vector(length(plconf), mode = "list")
        betahist.up <- vector(length(plconf), mode = "list")
        pl.conv <- matrix(0, length(plconf), 4)
        dimnames(pl.conv)[[1]] <- as.list(plconf)
        dimnames(pl.conv)[[2]] <- as.list(c("lower, loglik", 
            "lower, beta", "upper, loglik", "upper, beta"))
        LL.0 <- fit.full$loglik - qchisq(1 - alpha, 1)/2
        pl.iter <- matrix(0, k, 2)
        icount <- 0
        for (i in plconf) {
            icount <- icount + 1
            inter <- logistpl(x, y, beta, i, LL.0, firth, -1, 
                offset, weight, plcontrol)
            fit$ci.lower[i] <- inter$beta
            pl.iter[i, 1] <- inter$iter
            betahist.lo[[icount]] <- inter$betahist
            pl.conv.lower <- t(inter$conv)
            inter <- logistpl(x, y, beta, i, LL.0, firth, 1, 
                offset, weight, plcontrol)
            fit$ci.upper[i] <- inter$beta
            pl.iter[i, 2] <- inter$iter
            betahist.up[[icount]] <- inter$betahist
            pl.conv.upper <- t(inter$conv)
            pl.conv[icount, ] <- cbind(pl.conv.lower, pl.conv.upper)
            fit.i <- logistf.fit(x, y, weight = weight, offset = offset, 
                firth, col.fit = (1:k)[-i], control = control)
            fit$prob[i] <- pchisq(2 * (fit.full$loglik - 
                fit.i$loglik), 1, lower.tail=F)
            fit$method.ci[i] <- "Profile Likelihood"
        }
        fit$pl.iter <- pl.iter
        fit$betahist <- list(lower = betahist.lo, upper = betahist.up)
        fit$pl.conv <- pl.conv
    }
    names(fit$prob) <- names(fit$ci.upper) <- names(fit$ci.lower) <- names(fit$coefficients) <- dimnames(x)[[2]]
    if (dataout) {
        fit$data <- data
        fit$weights <- weight
    }
    attr(fit, "class") <- c("logistf")
    fit
}

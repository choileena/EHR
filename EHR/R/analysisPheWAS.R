#' Statistical Analysis for PheWAS
#'
#' Implement three commonly used statistical methods to analyze data for
#' Phenome Wide Association Study (PheWAS)
#'
#' Implements three commonly used statistical methods to analyze the associations 
#' between exposure (e.g., drug exposure, genotypes) and various phenotypes in 
#' PheWAS. Firth's penalized-likelihood logistic regression is the default method to 
#' avoid the problem of separation in logistic regression, which is often a problem 
#' when analyzing sparse binary outcomes and exposure. Logistic regression with 
#' likelihood ratio test and conventional logistic regression with Wald test can be 
#' also performed.
#'
#' @param method define the statistical analysis method from 'firth', 'glm', and
#' 'lr'. 'firth': Firth's penalized-likelihood logistic regression; 'glm':
#' logistic regression with Wald test, 'lr': logistic regression with likelihood
#' ratio test.
#' @param adjust define the adjustment method from 'PS','demo','PS.demo', and
#' 'none'. 'PS': adjustment of PS only; 'demo': adjustment of demographics only;
#' 'PS.demo': adjustment of PS and demographics; 'none': no adjustment.
#' @param Exposure define the variable name of exposure variable.
#' @param PS define the variable name of propensity score.
#' @param demographics define the list of demographic variables.
#' @param phenotypes define the list of phenotypes that need to be analyzed.
#' @param data define the data.
#'
#' @return
#' \item{estimate}{the estimate of log odds ratio.}
#' \item{stdError}{the standard error.}
#' \item{statistic}{the test statistic.}
#' \item{pvalue}{the p-value.}
#'
#' @templateVar author choibeck
#' @template auth
#'
#' @examples
#' ## use small datasets to run this example
#' data(dataPheWASsmall)
#' ## make dd.base with subset of covariates from baseline data (dd.baseline.small)
#' ## or select covariates with upper code as shown below
#' upper.code.list <- unique(sub("[.][^.]*(.).*", "", colnames(dd.baseline.small)) )
#' upper.code.list <- intersect(upper.code.list, colnames(dd.baseline.small))
#' dd.base <- dd.baseline.small[, upper.code.list]
#' ## perform regularized logistic regression to obtain propensity score (PS) 
#' ## to adjust for potential confounders at baseline
#' phenos <- setdiff(colnames(dd.base), c('id', 'exposure'))
#' data.x <- as.matrix(dd.base[, phenos])
#' glmnet.fit <- glmnet::cv.glmnet(x=data.x, y=dd.base[,'exposure'],
#'                                 family="binomial", standardize=TRUE,
#'                                 alpha=0.1)
#' dd.base$PS <- c(predict(glmnet.fit, data.x, s='lambda.min'))
#' data.ps <- dd.base[,c('id', 'PS')]
#' dd.all.ps <- merge(data.ps, dd.small, by='id')  
#' demographics <- c('age', 'race', 'gender')
#' phenotypeList <- setdiff(colnames(dd.small), c('id','exposure','age','race','gender'))
#' ## run with a subset of phenotypeList to get quicker results
#' phenotypeList.sub <- sample(phenotypeList, 5)
#' results.sub <- analysisPheWAS(method='firth', adjust='PS', Exposure='exposure',
#'                               PS='PS', demographics=demographics, 
#'                               phenotypes=phenotypeList.sub, data=dd.all.ps)
#' ## run with the full list of phenotype outcomes (i.e., phenotypeList)
#' \donttest{
#'         results <- analysisPheWAS(method='firth', adjust='PS',Exposure='exposure',
#'                           PS='PS', demographics=demographics,
#'                           phenotypes=phenotypeList, data=dd.all.ps) 
#'          }
#' @export

`analysisPheWAS` <- 
function(method=c('firth','glm', 'lr'), adjust=c('PS','demo','PS.demo','none'),
         Exposure, PS, demographics, phenotypes, data) {
    method <- match.arg(method)
    adjust <- match.arg(adjust)
    len <- length(phenotypes)
    outcome.tt <- matrix(NA, ncol=4, nrow=len)
    rownames(outcome.tt) <- phenotypes
    colnames(outcome.tt) <- c('estimate','stdError','statistic','pvalue')
    pred <- switch(adjust,
        PS = c(Exposure, PS),
        demo = c(Exposure, demographics),
        PS.demo = c(Exposure, PS, demographics),
        Exposure
    )
    rhs <- paste(pred, collapse = ' + ')
    form <- as.formula(sprintf("%s ~ %s", phenotypes[1], rhs))
    if(method == 'firth') {
        f.pml <- Logistf(form, data)
        for(i in seq(len)) {
            f.pml <- update(f.pml, paste(phenotypes[i], ' ~ .'))
            p <- f.pml$prob
            out <- cbind(coef(f.pml), diag(f.pml$var)^0.5, qchisq(1 - p, 1), p)
            outcome.tt[i,] <- out[Exposure,]
        }
    } else if(method == 'glm') {
        f.glm <- glm(form, data=data, family=quasibinomial())
        for(i in seq(len)) {
            f.glm <- update(f.glm, paste(phenotypes[i], ' ~ .'))
            outcome.tt[i,] <- summary(f.glm)$coef['exposure',]
        }
    } else {
        f.glm <- glm(form, data=data, family=binomial())
        rhsEx <- paste(setdiff(pred, Exposure), collapse = ' + ')
        if(rhsEx == "") {
            rhsEx <- '1'
        }
        for(i in seq(len)) {
            f.glm1 <- update(f.glm, paste(phenotypes[i], ' ~ .'))
            f.glm0 <- update(f.glm, paste(phenotypes[i], ' ~ ', rhsEx))
            outcome.tt[i, 1:3] <- summary(f.glm1)$coef['exposure', 1:3]
            q <- (logLik(f.glm1) - logLik(f.glm0))*2
            outcome.tt[i, 4] <- pchisq(q, 1, lower.tail=FALSE)
        }
    }
    data.frame(outcome.tt)
}

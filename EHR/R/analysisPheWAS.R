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

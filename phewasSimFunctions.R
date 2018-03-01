######################################################################
######################################################################
## Functions for simulation and analysis for PheWAS study
## Authors: Leena Choi and Cole Beck (February 2018)
## Reference: "Evaluating statistical approaches to leverage large clinical datasets for uncovering thera-peutic and adverse medication effects"
######################################################################
######################################################################

######################################################################
########## Exclude phenotypes with no case
##### Input:
#            data: phenotype data
#            ind: index where data columns from 1 to index do not contain phenotypes
##### Output:
#            data: data for which phenotypes columns with no case are excluded

excudeAllzeros <- function(data, ind){
    all.phenotypes <- colnames(data)[-c(1:ind)]
    tt.phenotypes <- matrix(NA, ncol=4, nrow=length(all.phenotypes), dimnames=list(all.phenotypes, c("00", "01", "10", "11")))
    for(i in seq_along(all.phenotypes)) {
        t <- zeroOneTable(data[, 'exposure'], data[, all.phenotypes[i]])
        if(!any(is.na(t))) {
            tt.phenotypes[i,] <- t
        }
    }
    valid.pheno <- rownames(tt.phenotypes)[!(is.na(tt.phenotypes[, "01"]) & is.na(tt.phenotypes[, "11"]))]
    dd.anytrue <- data[, c(colnames(data)[c(1:ind)], valid.pheno)]
    return(dd.anytrue)
}

######################################################################
########## define frequency for covariates and phenotypes
##### Input:
#            nc: the number of covariates
#            np: the number of phenotypes
#            mode: frequency condition (choice of high or low)
##### Output:
#            freq.pred: a vector of frequency for covariates
#            freq.pheno: a vector of frequency for phenotypes

makeFreq <- function(nc, np, mode=c('high','low')) {
    mode <- match.arg(mode)
    freq.fac <- 1
    if(mode == 'high') freq.fac <- 1.3
    pred <- c(0.289,0.077,0.046,0.027,0.019,0.014,0.013,0.011,0.008,0.007,0.006,0.003,0.002,0.001,0.001,0.000)* freq.fac
    phen <- c(0.1*c(0.364,0.036), 0.3*c(0.02,0.013), 0.5*c(0.009,0.008,0.006,0.004,0.004,0.003,0.003,0.001,0.001,0.000,0.000))
    ppred <- c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.05,0.05,0.05,0.05,0.10,0.60)
    pphen <- c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.05,0.05,0.05,0.05,0.7)
    f <- round(ppred[-1] * nc)
    num.pred.each.freq <- c(nc - sum(f), f)
    p <- round(pphen[-1] * np)
    num.pheno.each.freq <- c(np - sum(p), p)
    freq.vec.pred <- rep(pred, num.pred.each.freq)
    freq.vec.pheno <- rep(phen, num.pheno.each.freq)
    list(freq.pred=freq.vec.pred, freq.pheno=freq.vec.pheno)
}

######################################################################
########## Simulate data
##### Input:
#            dd.all: input data with covariates (e.g., the baseline phenotype data)
#            nc: the number of covariates
#            np: the number of phenotypes
#            freq.vec.pred: a vector of frequency for covariates
#            freq.vec.pheno: a vector of frequency for phenotypes
#            base.logodds: log odds that is defined  by exposure rate
#            phenotype.exposure: hypothesized log odds ratio (OR) for non-null exposure effects
#            prop.each.phenocoef: proportion of phenotypes with non-null effects
#            phenotype.cov.assoc.coef.all: estimated ß coefficients for the baseline covariates
#            phenotype.cov.assoc.sd: standard deviation in normal model for phenotypes coefficients
#            phenotype.demo.coef.sd: standard deviation in normal model for demographics coefficients
##### Output:
#            dd.O.all: simulated outcome phenotype data

simData <- function(dd.all, nc, np, freq.vec.pred, freq.vec.pheno, base.logodds,
    phenotype.exposure, prop.each.phenocoef, phenotype.cov.assoc.coef.all, phenotype.cov.assoc.sd, phenotype.demo.coef.sd) {
    Demo <- dd.all[,c('age','race','gender')]
    N <- nrow(dd.all)
    int <- rep(1, N)
    exposure <- dd.all$exposure

    ## phenotype model
    phenotype.exposure.all <- numeric(np)
    num.each.pheno <- round(np*prop.each.phenocoef)
    base.logodds.pheno <- phenotype.cov.assoc.coef.all[1, ]
    freq.vec.pheno <- 1/(1 + exp(-as.numeric(base.logodds.pheno)))
    phen.prob <- rep(1, np)
    phen.prob[freq.vec.pheno >= quantile(freq.vec.pheno, 0.85, na.rm = TRUE) ] = 20
    ic <- sample(np, sum(num.each.pheno), prob=phen.prob)

    ix <- split(ic, rep(seq_along(num.each.pheno), times=num.each.pheno))
    for(i in seq_along(num.each.pheno)) {
        phenotype.exposure.all[ix[[as.character(i)]]] <- phenotype.exposure[i]
    }

    sim.demo.coef <- apply(phenotype.cov.assoc.coef.all[2:4,], 2, function(a) rnorm(3, a, sd=phenotype.demo.coef.sd))
    sim.phenotype.cov.assoc.coef <- apply(phenotype.cov.assoc.coef.all[5:nrow(phenotype.cov.assoc.coef.all),], 2, function(a) rnorm(nc, a, sd=phenotype.cov.assoc.sd))
    phenotype.coef <- rbind(base.logodds.pheno, phenotype.exposure.all, sim.demo.coef, sim.phenotype.cov.assoc.coef)
    Z <- as.matrix(cbind(int, dd.all[,-1]))
    Zb <- Z %*% as.matrix(phenotype.coef)
    phenotype <- apply(Zb, MARGIN=2, function(p) {
        rbinom(N, 1, prob = 1 / (1 + exp(-p)))
    })
    colnames(phenotype) <- paste0("X", seq(np))
    d.outcome <- data.frame(id=seq(N), exposure=exposure, Demo, phenotype)

    return(d.outcome)
}

######################################################################
########## PS model
##### Input:
#            alpha.para: alpha parameter for glmnet
#            data: input data with covariates (e.g., the baseline phenotype data)
##### Output:
#            data.ps: output data with generated propensity score (PS) and demographic data

psModel <- function(alpha.para, data) {
    if(is.null(alpha.para)) {
        data2 <- data[,-1]
        na.coef <- 1
        while(na.coef != 0) {
            f0 <- glm(exposure ~ ., data2, family = quasibinomial(link='logit'))
            (na.coef <- sum(is.na(f0$coefficient)))
            na.list <- names(f0$coefficient[is.na(f0$coefficient)])
            covariates.list <- names(data2)
            no.na.covariates <- covariates.list[!covariates.list%in%na.list]
            data2 <- data2[,no.na.covariates]
        }
        f1 <- glm(exposure ~ ., data2, family = quasibinomial(link='logit'))
        ps <- predict(f1)
    } else {
        data.x <- as.matrix(data[,-c(1:2)])
        ll.glmnet <- cv.glmnet(x=data.x, y=data$exposure, family="binomial", standardize=TRUE, alpha=alpha.para)
        ps <- predict(ll.glmnet, data.x, s='lambda.min')
    }
    data.ps <- cbind(data[,c('id','exposure','age','gender','race')], PS=ps)
    colnames(data.ps)[6] <- "PS"
    return(data.ps)
}

##########################################################################
## End
##########################################################################

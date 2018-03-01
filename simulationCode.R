######################################################################
######################################################################
## Simulation Script (February, 2018)
## Authors: Leena Choi and Cole Beck
## Reference: "Evaluating statistical approaches to leverage large clinical datasets for uncovering thera-peutic and adverse medication effects"
## This script consists of 3 sections:
## (I) Preprocessing baseline data, (II) Simulation, and (III) Analysis using the simulated data
######################################################################
######################################################################

######################################################################
## R packages and functions for simulation and analysis
######################################################################
library(glmnet)
library(EHR)
source('phewasSimFunctions.R')

######################################################################
## (I) Preprocessing baseline data
## Figure 2 in the reference
######################################################################
# read in the baseline data
# see an example baseline data, "dd.baseline" in R package "EHR"
# data(dataPheWAS)
# > dd.baseline[1:5, 1:10]
  # id exposure age race gender X008 X008.5 X008.51 X008.52 X008.6
# 2  1        0  17    0      1    0      0       0       0      0
# 3  2        0   5    0      1    0      0       0       0      0
# 4  3        0   5    1      1    0      0       0       0      0
# 5  4        0  20    1      0    0      0       0       0      0

## define covariate list with upper code only
demographics <- c('age', 'race', 'gender')
demo.covariates <- c('id','exposure', demographics)
all.covariates <- setdiff(colnames(dd.baseline), demo.covariates)
upper.code.list <- unique(sub("[.][^.]*(.).*", "", all.covariates))
upper.code.list <- intersect(upper.code.list, all.covariates)

## make contingency tables for phenotypes and drug exposure to remove phenotypes with no case
tt.baseline <- matrix(NA, ncol=6, nrow=length(upper.code.list),
    dimnames=list(upper.code.list, c("n.nocase.nonexp", "n.case.nonexp", "n.nocase.exp", "n.case.exp", "n.case.total", "n.total")))
tt.baseline <- as.data.frame(tt.baseline, stringsAsFactors = FALSE)
for(i in seq_along(upper.code.list)) {
    t <- zeroOneTable(dd.baseline[, 'exposure'], dd.baseline[, upper.code.list[i]])
    tt.baseline[i,1:4] <- c(t)
    tt.baseline[i,"n.total"] <- sum(t, na.rm=TRUE)
    if(sum(!is.na(t)) == 4) {
        tt.baseline[i,"n.case.total"] <- t[2] + t[4]
    } else {
        tt.baseline[i,"n.case.total"] <- 0
    }
}
tt.baseline <- tt.baseline[!(is.na(tt.baseline[, "n.case.nonexp"]) & is.na(tt.baseline[, "n.case.exp"])), ]
baseline.covariates <- row.names(tt.baseline)
num.covariates <- length(baseline.covariates)

## generate the preprocessed baseline data
dd.all <- dd.baseline[, c(demo.covariates, baseline.covariates)]
dd.all$age <- (dd.all$age - mean(dd.all$age))/sd(dd.all$age)
dd.all <- dd.all*1
dd.all$id <- seq(nrow(dd.all))
dim(dd.all)

######################################################################
## (II) Simulation
## Figure 3 in the reference
######################################################################

num.phenotype <- 761 # the number of outcome phenotypes to be simulated
#### Step 1: prepare "phenotype.cov.assoc.coef.all" that is nc x np matrix, where nc(=num.covariates) is the number of baseline phenotype, and np(=num.phenotype) is the number of outcome phenotypes
## note that this nc x np matrix can be generated in two ways
# (A) as described in Step 1 of Figure 3, estimate the coefficients ßj using the baseline and the outcome phenotypes datasets, i.e., j=1, ..., np, for each jth outcome phenotype, obtain a vector of the estimated ßj [nc x 1] for the baseline covariates. Iteration of np outcome phenotypes will yield nc x np matrix, called as "phenotype.cov.assoc.coef.all"
# (B) or based on frequency and structure of specific data type, assume the values for the coefficients in nc x np matrix, called as "phenotype.cov.assoc.coef.all" 
dim(phenotype.cov.assoc.coef.all) # nc x np matrix

#### Step 2 - 5: simulate outcome phenotypes data
## simulation condition
freq <- makeFreq(num.covariates, num.phenotype, mode='low')
freq.vec.pred <- freq$freq.pred
freq.vec.pheno <- freq$freq.pheno
base.logodds <- log(0.11/(1-0.11))
phenotype.exposure <- log(c(3, 2))
prop.each.phenocoef <- c(0.015, 0.015)
phenotype.cov.assoc.sd <- 0.1
demo.coef.sd <- c(0.05, 0.5, 0.5)

## simulate outcome phenotype data
set.seed(10)
dd.O.all <- simData(dd.all=dd.all, nc=num.covariates, np=num.phenotype, freq.vec.pred=freq.vec.pred, freq.vec.pheno=freq.vec.pheno, base.logodds=base.logodds, phenotype.exposure=phenotype.exposure, prop.each.phenocoef=prop.each.phenocoef, phenotype.cov.assoc.coef.all=phenotype.cov.assoc.coef.all, phenotype.cov.assoc.sd=phenotype.cov.assoc.sd, phenotype.demo.coef.sd=demo.coef.sd)

######################################################################
## (III) Analysis using the simulated data
## See the reference for the analysis methods
######################################################################

#### generate PS and make the data for PheWAS analysis
data.ps.alpha1.all <- psModel(alpha.para=0.1, data=dd.all)
dd.O.ps <- merge(data.ps.alpha1.all[, c('id','PS')], dd.O.all, by='id')
dd.O.ps <- excudeAllzeros(data=dd.O.ps, ind=(length(demo.covariates)+1) )
phenotypeList <- setdiff(colnames(dd.O.ps), c(demo.covariates,'PS'))

#### PheWAS analysis
results <- analysisPheWAS(method='firth', adjust='PS',Exposure='exposure', PS='PS', demographics=demographics, phenotypes=phenotypeList, data=dd.O.ps) 

######################################################################
######################################################################
## End of Script
######################################################################
######################################################################

######################################################################
######################################################################
## Data Processing and Analysis for PheWAS (Oct. 20, 2017)
## Authors: Leena Choi, Cole Beck, and Robert J. Carroll
######################################################################
######################################################################
library(EHR)
library(glmnet)

######################################################################
## Data Processing
######################################################################

## load a simulated example PheWAS data from EHR package
## all variables should be coded as numeric values (no character strings are allowed in data):
## i.g., values for TRUE or FALSE should changed to numeric values
data(dataPheWAS)

## define list of exposure and demographic covariates for your data
## e.g., 'exposure' is the name of variable for Exposure
## 'age','race','gender' are the names of demographic variables (demographics)
## 'id' is the name of ID variable
Exposure <- 'exposure'
ID <- 'id'
demographics <- c('age','race','gender')
demo.covariates <- c(ID, Exposure,demographics) 

######################################################################
#### covariate (baseline) data preprocessing
all.covariates <- setdiff(colnames(dd.baseline), demo.covariates)
## define covariate list with upper code only
upper.code.list <- unique(sub("[.][^.]*(.).*", "", all.covariates) ) 
upper.code.list <- intersect(upper.code.list, all.covariates)

## make contingency tables for phenotypes and drug exposure to preprocess baseline phenotype data
## where phenotypes without any cases are removed
tt.baseline <- matrix(NA, ncol=6, nrow=length(upper.code.list), dimnames=list(upper.code.list, c("n.nocase.nonexp", "n.case.nonexp", "n.nocase.exp", "n.case.exp", "n.case.total", "n.total")))
tt.baseline <- as.data.frame(tt.baseline, stringsAsFactors = FALSE)
for(i in seq_along(upper.code.list)) {
    t <- zeroOneTable(dd.baseline[, Exposure], dd.baseline[, upper.code.list[i]])
    tt.baseline[i,1:4] <- c(t)
    tt.baseline[i,"n.total"] <- sum(t, na.rm=TRUE)
    if(!any(is.na(t))) {
        tt.baseline[i,"n.case.total"] <- t[2] + t[4]
    } else {
        tt.baseline[i,"n.case.total"] <- 0
    }
}

## remove phenotypes that do no have any cases
tt.baseline <- tt.baseline[!is.na(tt.baseline[, "n.case.nonexp"]) | !is.na(tt.baseline[, "n.case.exp"]), ]
baseline.covariates <- row.names(tt.baseline)

## dd.base is the final preprocessed baseline data
dd.base <- dd.baseline[, c(demo.covariates, baseline.covariates)]
## standarize age variable
dd.base$age <- (dd.base$age - mean(dd.base$age)) / sd(dd.base$age)

#### outcome phenotype data preprocessing
phenotypeList <- setdiff(colnames(dd), demo.covariates)

## make contingency tables for phenotypes and drug exposure to preprocess outcome phenotype data
## where phenotypes without any cases or only 1 case are removed
tt.phenotype <- matrix(NA, ncol=6, nrow=length(phenotypeList), dimnames=list(phenotypeList, c("n.nocase.nonexp", "n.case.nonexp", "n.nocase.exp", "n.case.exp", "n.case.total", "n.total")))
tt.phenotype <- as.data.frame(tt.phenotype, stringsAsFactors = FALSE)
for(i in seq_along(phenotypeList)) {
    t <- zeroOneTable(dd[, Exposure], dd[, phenotypeList[i]])
    tt.phenotype[i,1:4] <- c(t)
    tt.phenotype[i,"n.total"] <- sum(t, na.rm=TRUE)
    if(!any(is.na(t))) {
        tt.phenotype[i,"n.case.total"] <- t[2] + t[4]
    } else {
        tt.phenotype[i,"n.case.total"] <- 0
    }
}

## remove phenotypes that do no have any cases or only 1 case
tt.phenotype <- tt.phenotype[!is.na(tt.phenotype$n.case.total) & tt.phenotype$n.case.total > 1, ]
phenotype.final.list <- row.names(tt.phenotype)
dd.outcome <- dd[, c(demo.covariates, phenotype.final.list)]

######################################################################
## Data Analysis
######################################################################

#### predict PS and generate the final outcome data for PheWAS
set.seed(100)
alpha.para <- 0.1
## set up covarite matrix that exclude 'ID' and 'Exposure'
data.x <- as.matrix(dd.base[, setdiff(colnames(dd.base), c(ID, Exposure))]  )
glmnet.fit <- cv.glmnet(x=data.x, y=dd.base[,Exposure], family="binomial", standardize=TRUE, alpha=alpha.para)
dd.base$PS <- c(predict(glmnet.fit, data.x, s='lambda.min'))
data.ps <- dd.base[,c(ID, 'PS')]
dd.all.ps <- merge(data.ps, dd.outcome, by=ID)  

#### data analysis
## 3 commonly used statistical analysis methods are available in analysisPheWAS()
## 'firth': Firth's penalized-likelihood logistic regression; 'glm': logistic regression with Wald test, 'lr': logistic regression with likelihood ratio test
## 3 adjustment methods: 'PS': adjustment of PS only; 'demo': adjustment of demographics only; 'PS.demo': adjustment of PS and demographics; 'none': no adjustment
## default method is Firth's penalized-likelihood logistic regression with 'PS' adjustment

demographics <- c('age', 'race', 'gender')
phenotypeList <- phenotype.final.list
results <- analysisPheWAS(method='firth', adjust='PS',Exposure='exposure', PS='PS', demographics=demographics, phenotypes=phenotypeList, data=dd.all.ps) 

######################################################################
## End of Data Processing and Analysis
######################################################################

######################################################################
## Manhattan Plot using PheWAS Package
######################################################################
## install an R package 'PheWAS'
## instruction for installation:see https://github.com/PheWAS
library(PheWAS)

## prepare appropriate format for results to make Manhattan plot using PheWAS package
results$phenotype <- as.character(gsub('[^0-9.]', '', rownames(results)))
## make OR if only log odds ratio (OR) is in the results
results$OR <- exp(results$estimate)
## make phenotype if phenotype is not already in the results
tt.phenotype$phenotype <- as.character(gsub('[^0-9.]', '', rownames(tt.phenotype)))
## formated results with all required variables
results.pheno <- merge(results, tt.phenotype)

## remove phenotypes that are unreasonable for associations (e.g., 'congenital anomalies' are impossible phenotypes to be associated)
results.pheno.final = results.pheno %>%  addPhecodeInfo() %>% filter(group!='congenital anomalies',!grepl("congenital",description,ignore.case=T)) %>% arrange(pvalue)
man <- results.pheno.final[,-3] %>% transmute(phenotype=phecode, OR, p=pvalue, n_cases=n.case.total, description)
man$labelpick <- NA
## pick phenotypes having at least 50 cases, which will be labeled
case_cut <- 50
man$labelpick[man$p < 0.05 & man$n_cases > case_cut] <- man$description[man$p < 0.05 & man$n_cases > case_cut]
man <- man[,-5]
## make a list for phenotypes that will be labeled
annotate.list <- man$phenotype[is.na(man$labelpick)==0]
## the maximum value of y-axis
ymax <- 3 # or use -log10(min(man$p))

## make Manhattan plot
plot1 <- phewasManhattan(man, annotate.angle = 0, annotate.size = 5, annotate.level = 4e-33, title = "", y.axis.interval = 1, OR.direction=T, significant.line=NA, max.y=ymax, annotate.list=annotate.list) + aes(size=cut(n_cases,breaks=c(0, 50,100,200,500, 100000),right=FALSE)) + scale_size_discrete(range=c(2,6),guide="legend") + guides(size="legend")

## save the Manhattan plot
ggsave(plot1, filename = "plotmanhattan.pdf",width = 8, height=4.5,scale = 2)

######################################################################
######################################################################
## End of Script
######################################################################
######################################################################

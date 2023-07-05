library(EHR)

data(dataPheWASsmall)
upper.code.list <- unique(sub("[.][^.]*(.).*", "", colnames(dd.baseline.small)) )
upper.code.list <- intersect(upper.code.list, colnames(dd.baseline.small))
dd.base <- dd.baseline.small[, upper.code.list]

phenos <- setdiff(colnames(dd.base), c('id', 'exposure'))
data.x <- as.matrix(dd.base[, phenos])
glmnet.fit <- glmnet::cv.glmnet(x=data.x, y=dd.base[,'exposure'], family="binomial", standardize=TRUE, alpha=0.1)
dd.base$PS <- c(predict(glmnet.fit, data.x, s='lambda.min'))
data.ps <- dd.base[,c('id', 'PS')]
dd.all.ps <- merge(data.ps, dd.small, by='id')  
demographics <- c('age', 'race', 'gender')
phenotypeList <- setdiff(colnames(dd.small), c('id', 'exposure', demographics))
set.seed(5)
phenotypeList.sub <- sample(phenotypeList, 5)

suppressWarnings(r1 <- analysisPheWAS(method='firth', adjust='PS', Exposure='exposure',
  PS='PS', demographics=demographics, phenotypes=phenotypeList.sub, data=dd.all.ps
))
suppressWarnings(r2 <- analysisPheWAS(method='glm', adjust='demo', Exposure='exposure',
  PS='PS', demographics=demographics, phenotypes=phenotypeList.sub, data=dd.all.ps
))
suppressWarnings(r3 <- analysisPheWAS(method='lr', adjust='PS.demo', Exposure='exposure',
  PS='PS', demographics=demographics, phenotypes=phenotypeList.sub, data=dd.all.ps
))
suppressWarnings(r4 <- analysisPheWAS(method='lr', adjust='none', Exposure='exposure',
  PS='PS', demographics=demographics, phenotypes=phenotypeList.sub, data=dd.all.ps
))

expect_equal(phenotypeList.sub, rownames(r1))
expect_equal(phenotypeList.sub, rownames(r2))
expect_equal(phenotypeList.sub, rownames(r3))
expect_equal(phenotypeList.sub, rownames(r4))
expect_equal(c(FALSE, FALSE, FALSE, FALSE, FALSE), r1$pvalue < 0.1)
expect_equal(c(FALSE, FALSE, TRUE, FALSE, FALSE), r2$pvalue < 0.1)
expect_equal(c(FALSE, FALSE, TRUE, FALSE, FALSE), r3$pvalue < 0.1)
expect_equal(c(FALSE, FALSE, TRUE, FALSE, FALSE), r4$pvalue < 0.1)

t01 <- zeroOneTable(dd.all.ps$exposure, dd.all.ps$X382)
expect_equal(c(1719,30,248,3), t01)
t01 <- zeroOneTable(dd.all.ps$exposure, dd.all.ps$X796)
expect_equal(c(1749,NA,251,NA), t01)

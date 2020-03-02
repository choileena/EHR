`zeroOneTable` <- 
function(EXPOSURE, phenotype) {
    t00 <- sum(!EXPOSURE & !phenotype)
    t01 <- sum(!EXPOSURE & phenotype)
    t10 <- sum(EXPOSURE & !phenotype)
    t11 <- sum(EXPOSURE & phenotype)
    if(!t00 & !t01) t00 <- t01 <- NA
    if(!t10 & !t11) t10 <- t11 <- NA
    if(!t00 & !t10) t00 <- t10 <- NA
    if(!t01 & !t11) t01 <- t11 <- NA
    c(t00, t01, t10, t11)
}

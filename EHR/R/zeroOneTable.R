#' Make Zero One Contingency Tables
#'
#' Make contingency tables for many binary outcomes and a binary covariate
#'
#' Generates frequency and contingency tables for many binary outcomes (e.g.,
#' large number of phenotypes) and a binary covariate (e.g., drug exposure,
#' genotypes) more efficiently.
#'
#' @param EXPOSURE binary covariate (e.g., exposure).
#' @param phenotype binary outcome (e.g., phenotype).
#'
#' @return
#' \item{t00}{frequency for non-exposed group and non-case outcome.}
#' \item{t01}{frequency for non-exposed group and case outcome.}
#' \item{t10}{frequency for exposed group and non-case outcome.}
#' \item{t11}{frequency for exposed group and case outcome.}
#'
#' @templateVar author choibeck
#' @template auth
#'
#' @examples
#' ## full example data
#' data(dataPheWAS)
#' demo.covariates <- c('id','exposure','age','race','gender')
#' phenotypeList <- setdiff(colnames(dd), demo.covariates)
#' tablePhenotype <- matrix(NA, ncol=4, nrow=length(phenotypeList), 
#' dimnames=list(phenotypeList, c("n.nocase.nonexp", "n.case.nonexp", 
#' "n.nocase.exp", "n.case.exp")))
#' for(i in seq_along(phenotypeList)) {
#'     tablePhenotype[i, ] <- zeroOneTable(dd[, 'exposure'], dd[, phenotypeList[i]])
#' }
#' @export

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

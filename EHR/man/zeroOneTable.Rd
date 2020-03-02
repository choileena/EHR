\name{zeroOneTable}
\alias{zeroOneTable}
\title{
  Make Zero One Contingency Tables
}
\description{
  Make contingency tables for many binary outcomes and a binary covariate}
\usage{
  zeroOneTable(EXPOSURE, phenotype)
}
\arguments{
  \item{EXPOSURE}{
  binary covariate (e.g., exposure).}
  \item{phenotype}{
  binary outcome (e.g., phenotype).}
}
\details{
  Generates frequency and contingency tables for many binary outcomes (e.g.,
  large number of phenotypes) and a binary covariate (e.g., drug exposure, 
  genotypes) more efficiently.
}

\value{
\item{t00}{frequency for non-exposed group and non-case outcome.}
\item{t01}{frequency for non-exposed group and case outcome.}
\item{t10}{frequency for exposed group and non-case outcome.}
\item{t11}{frequency for exposed group and case outcome.}
}

\author{
  Leena Choi <naturechoi@gmail.com> and Cole Beck <cole.beck@Vanderbilt.Edu>
}

\examples{
## full example data
data(dataPheWAS)
demo.covariates <- c('id','exposure','age','race','gender')
phenotypeList <- setdiff(colnames(dd), demo.covariates)
tablePhenotype <- matrix(NA, ncol=4, nrow=length(phenotypeList), 
dimnames=list(phenotypeList, c("n.nocase.nonexp", "n.case.nonexp", 
"n.nocase.exp", "n.case.exp")))
for(i in seq_along(phenotypeList)) {
    tablePhenotype[i, ] <- zeroOneTable(dd[, 'exposure'], dd[, phenotypeList[i]])
}
}

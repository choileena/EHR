% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modMedStr2.R
\name{run_MedStrII}
\alias{run_MedStrII}
\title{Run Structured E-Prescription Data}
\usage{
run_MedStrII(file, dat.columns = list())
}
\arguments{
\item{file}{filename of prescription data (CSV, RData, RDS), or data.frame}

\item{dat.columns}{a named list that should specify columns in data; \sQuote{id},
\sQuote{dose}, \sQuote{freq}, \sQuote{date}, and \sQuote{str} are required.
\sQuote{desc} may also be specified.}
}
\value{
str data set
}
\description{
This module will load and modify structured e-prescription data.
}
\details{
See EHR Vignette for Structured Data.
}
\examples{
erx_data <- data.frame(GRID=paste0("ID",c(1,1,2,2,2,2)),
                       MED_NAME=c("fakedrug","fakedrug","fakedrug",
                                  "Brandname","fakedrug","fakedrug"),
                       RX_DOSE=c(1,2,1,'2 tabs',1,'1+1.5+1'),
                       FREQUENCY=c(rep("bid",3),"qam","bid",
                                   "brkfst,lunch,dinner"),
                       ENTRY_DATE=c("2018-02-15","2018-03-14","2017-07-01",
                                    "2017-07-01","2017-09-15","2017-11-01"),
                       STRENGTH_AMOUNT=c("100","100","200",
                                         "100mg","100","100"),
                       DESCRIPTION=c("fakedrug 100 mg tablet","fakedrug 100 mg tablet",
                                     "fakedrug 200 mg tablet (also known as brandname)",
                                     "Brandname 100mg tablet", "fakedrug 100 mg tablet",
                                     "fakedrug 100 mg tablet"))

run_MedStrII(erx_data, list(id = 'GRID', dose = 'RX_DOSE', freq = 'FREQUENCY',
             date = 'ENTRY_DATE', str = 'STRENGTH_AMOUNT', desc = 'DESCRIPTION'))

}

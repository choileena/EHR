% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilsParse.R
\name{parse-internal}
\alias{parse-internal}
\alias{medxnColonFormat}
\alias{entorder}
\alias{medxnEntityFormat}
\title{Internal functions for parse process}
\description{
These internal functions aid the main functions used in the parsing process 
(\code{\link{parseMedExtractR}}, \code{\link{parseMedXN}},
\code{\link{parseMedEx}}, \code{\link{parseCLAMP}}).
}
\details{
\code{medxnColonFormat}: converts entity information into the 
form "extracted expression::start position::stop position", similar 
to how output is formatted by default in MedXN output.  

\code{entorder}: a helper function that orders the entities by start position  

\code{medxnEntityFormat}: a helper function that applies both \code{medxnColonFormat} 
to convert entities into the "extraction::start::stop" format and \code{entorder} to 
sort them. It then collapses the entities and separates the extractions with backticks
}
\keyword{internal}

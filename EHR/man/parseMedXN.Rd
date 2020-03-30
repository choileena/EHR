% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseMedxn.R
\name{parseMedXN}
\alias{parseMedXN}
\title{Parse MedXN NLP Output}
\usage{
parseMedXN(filename, begText = "^[R0-9]+_[0-9-]+_[0-9]+_")
}
\arguments{
\item{filename}{File name for single file containg MedXN output.}

\item{begText}{A regular expression that would indicate the beginning of a new
observation (i.e., extracted clinic note).}
}
\value{
A data.table object with columns for filename, drugname, strength, dose, route, 
freq, and duration. The filename contains the file name corresponding to the clinical 
note. Each of the entity columns are of the format 
"extracted expression::start position::stop position".
}
\description{
Take files with the raw medication extraction output generated by the MedXN 
natural language processing system and converts it into a standardized format.
}
\details{
Output from different medication extraction systems is formatted in different ways.
In order to be able to process the extracted information, we first need to convert 
the output from different systems into a standardized format. Extracted expressions 
for various drug entities (e.g., drug name, strength, frequency, etc.) each receive 
their own column formatted as "extracted expression::start position::stop position". 
MedXN output files anchor extractions to a specific drug name extraction. In the case 
where multiple entities are extracted for the same drug name mention, the entities are 
separated into adjacent rows and the drug name is duplicated.

In MedXN output files, the results from multiple clinical notes can be combined into 
a single output file. The beginning of some lines of the output file can indicate 
when output for a new observation (or new clinical note) begins. The user should specify 
the argument \code{begText} to be a regular expression used to identify the lines where output 
for a new clinical note begins.
}
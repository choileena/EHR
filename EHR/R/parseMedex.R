#' Parse MedEx NLP Output
#'
#' Takes files with the raw medication extraction output generated by the MedEx
#' natural language processing system and converts it into a standardized format.
#'
#' Output from different medication extraction systems is formatted in different ways.
#' In order to be able to process the extracted information, we first need to convert
#' the output from different systems into a standardized format. Extracted expressions
#' for various drug entities (e.g., drug name, strength, frequency, etc.) each receive
#' their own column formatted as "extracted expression::start position::stop position".
#' If multiple expressions are extracted for the same entity, they will be separated by
#' backticks.
#'
#' MedEx output files anchor extractions to a specific drug name extraction.
#'
#' See EHR Vignette for Extract-Med and Pro-Med-NLP as well as Dose Building Using Example Vanderbilt EHR Data for details.
#'
#' @param filename File name for a single file containing MedEx output.
#'
#' @return A data.table object with columns for filename, drugname, strength, dose, route,
#' and freq. The filename contains the file name corresponding to the clinical
#' note. Each of the entity columns are of the format
#' "extracted expression::start position::stop position".
#' @export

parseMedEx <- function(filename) {
  con <- file(filename, 'r', blocking = TRUE)
  l <- readLines(con)
  close(con)
  if(length(l) == 0) return(NULL)
  md <- do.call(rbind, lapply(strsplit(l, '|', fixed = TRUE), function(i) {
    i[-seq(length(i) - 4)]
  }))
  md <- md[md[,2] %in% c('DBN','DIN','DPN','DOSE','DOSEAMT','FREQ','RUT'),,drop = FALSE]
  df <- data.frame(entity = md[,2], value = md[,1], start = as.numeric(md[,3]),
                   stop = as.numeric(md[,4]), stringsAsFactors = FALSE)

  ix <- which(df[['entity']] %in% c('DBN', 'DIN','DPN'))
  if(length(ix) == 0) return(NULL)
  bord <- c(ix[-1]-1, nrow(df))
  l <- length(ix)
  cast <- character(l)
  drugname <- cast
  strength <- cast
  dose <- cast
  freq <- cast
  route <- cast
  for(i in seq(l)) {
    tmp <- df[seq(ix[i], bord[i]),]
    strings <- do.call(paste, c(tmp[,-1], sep = ':'))
    drugname[i] <- strings[1]
    attr.s <- strings[tmp[['entity']] == 'DOSE']
    attr.d <- strings[tmp[['entity']] == 'DOSEAMT']
    attr.f <- strings[tmp[['entity']] == 'FREQ']
    attr.r <- strings[tmp[['entity']] == 'RUT']
    strength[i] <- medxnEntityFormat(attr.s)
    dose[i] <- medxnEntityFormat(attr.d)
    freq[i] <- medxnEntityFormat(attr.f)
    route[i] <- medxnEntityFormat(attr.r)
  }
  # need double-colon
  drugname <- medxnColonFormat(drugname)
  x <- data.frame(filename = basename(filename), drugname, strength, dose, route, freq, stringsAsFactors = FALSE)
  data.table::as.data.table(x)
}

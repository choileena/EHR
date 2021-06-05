#' Combine Dose Data
#'
#' Output from parse process is taken and converted into a wide format, grouping drug entity
#' information together based on various steps and rules.
#'
#' The \code{buildDose} function takes as its main input (\code{dat}), a data.table object that
#' is the output of a parse process function (\code{\link{parseMedExtractR}}, \code{\link{parseMedXN}},
#' \code{\link{parseMedEx}}, or \code{\link{parseCLAMP}}). Broadly, the parsed extractions are grouped
#' together to form wide, more complete drug regimen information. This reformatting facilitates
#' calculation of dose given intake and daily dose in the \code{\link{collapseDose}} process.
#'
#' The process of creating this output is broken down into multiple steps:
#' \enumerate{
#'   \item Removing rows for any drugs not of interest. Drugs of interest are specified with the \code{dn} argument.
#'   \item Determining whether extractions are "simple" (only one drug mention and at most one extraction per entity) or
#'   complex. Complex cases can be more straightforward if they contain at most one extraction per entity, or require a
#'   pairing algorithm to determine the best pairing if there are multiple extractions for one or more entities.
#'   \item Drug entities are anchored by drug name mention within the parse process. For complex cases, drug entities are
#'   further grouped together anchored at each strength (and dose with medExtractR) extraction.
#'   \item For strength groups with multiple extractions for at least one entity, these groups go through a path searching
#'   algorithm, which computes the cost for each path (based on a chosen distance method) and chooses the path with the
#'   lowest cost.
#'   \item The chosen paths for each strength group are returned as the final pairings. If route is unique within a strength
#'   group, it is standardized and added to all entries for that strength group.
#' }
#'
#' The user can specify additional arguments including:
#' \itemize{
#'  \item \code{dist_method}: The distance method is the metric used to determine which entity path is the most likely to
#'  be correct based on minimum cost.
#'  \item \code{na_penalty}: NA penalties are incurred when extractions are paired with nothing (i.e., an NA), requiring that
#'  entities be sufficiently far apart from one another before being left unpaired.
#'  \item \code{neg_penalty}: When working with dose amount (DA) and frequency/intake time (FIT), it is much more common
#' for the ordering to be DA followed by FIT. Thus, when we observe FIT followed by DA, we apply a negative penalty to make such pairings
#' less likely.
#'  \item \code{greedy threshold}: When there are many extractions from a clinical note, the number of possible combinations for paths
#'  can get exponentially large, particularly when the medication extraction natural language processing system is incorrect. The greedy
#'  threshold puts an upper bound on the number of entity pairings to prevent the function from stalling in such cases.
#' }
#' If none of the optional arguments are specified, then the \code{buildDose} process uses the default option values specified in the EHR
#' package documentation. 
#' See EHR Vignette for Extract-Med and Pro-Med-NLP as well as Dose Building Using Example Vanderbilt EHR Data for details. For additional details, see McNeer, et al. 2020.
#'
#' @param dat data.table object from the output of \code{\link{parseMedExtractR}},
#' \code{\link{parseMedXN}}, \code{\link{parseMedEx}}, or \code{\link{parseCLAMP}}
#' @param dn Regular expression specifying drug name(s) of interest.
#' @param preserve Column names to include in output, whose values should not be combined with other rows.
#' If present, dosechange is always preserved.
#' @param dist_method Distance method to use for calculating distance of various paths. Alternatively set the
#' \sQuote{ehr.dist_method} option, which defaults to \sQuote{minEntEnd}.
#' @param na_penalty Penalty for matching extracted entities with NA. Alternatively set the\cr
#' \sQuote{ehr.na_penalty} option, which defaults to 32.
#' @param neg_penalty Penalty for negative distances between frequency/intake time and dose amounts. Alternatively
#' set the \sQuote{ehr.neg_penalty} option, which defaults to 0.5.
#' @param greedy_threshold Threshold to use greedy matching; increasing this value too high could lead to the
#' algorithm taking a long time to finish. Alternatively set the\cr
#' \sQuote{ehr.greedy_threshold} option, which defaults to 1e8.
#' @param checkForRare Indicate if rare values for each entity should be found and displayed.
#'
#' @return A data.frame object that contains columns for filename (of the clinical note, inherited from the
#' parse output object \code{dat}), drugname, strength, dose, route, freq, duration, and drugname_start.
#'
#' @examples
#' data(lam_mxr_parsed)
#'
#' buildDose(lam_mxr_parsed)
#' @export

buildDose <- function(dat, dn = NULL, preserve = NULL, dist_method, na_penalty, neg_penalty, greedy_threshold, checkForRare = FALSE) {
  if(!missing(dist_method) || !missing(na_penalty) || !missing(neg_penalty) || !missing(greedy_threshold)) {
    opt_name <- c('ehr.dist_method','ehr.na_penalty','ehr.neg_penalty','ehr.greedy_threshold')
    curopts <- options()[opt_name]
    on.exit(options(curopts))
    if(!missing(dist_method)) options(ehr.dist_method = dist_method)
    if(!missing(na_penalty)) options(ehr.na_penalty = na_penalty)
    if(!missing(neg_penalty)) options(ehr.neg_penalty = neg_penalty)
    if(!missing(greedy_threshold)) options(ehr.greedy_threshold = greedy_threshold)
  }
  # NSE fix for R CMD CHECK
  drugname <- NULL
  drugname_start <- NULL
  filename <- NULL
  ..xnames <- NULL
  rm(drugname, drugname_start, filename, ..xnames)
  # end
  if(inherits(dat, 'data.frame') && !inherits(dat, 'data.table')) {
    dat <- data.table::as.data.table(dat)
  }
  if(!is.null(dn)) {
    x <- data.table::copy(dat[grepl(dn, drugname, ignore.case = TRUE)])
  } else {
    x <- data.table::copy(dat)
  }
  rm(dat)
  # deep copy of data.table column names
  xnames <- data.table::copy(names(x))
  preserve <- unique(c(preserve, 'dosechange'))
  preserve <- intersect(preserve, xnames)
  if(length(preserve) == 0L) {
    preserve <- NULL
  }

  # rows with dosestr
  if('dosestr' %in% xnames) {
    ixA <- which(x[['dosestr']] != '' & (x[['strength']] != '' | x[['dose']] != ''))
  } else {
    ixA <- numeric(0)
  }

  # rows with multiple entities
  ixB <- unlist(lapply(x[,-1], grep, pattern = '`'))
  ix <- sort(unique(c(ixA, ixB)))
  if(length(ix) == 0) {
    x1 <- x
    x3 <- NULL
  } else {
    x1 <- x[-ix]
    x2 <- x[ix]

    res <- vector('list', nrow(x2))
    for(i in seq_along(res)) {
      res[[i]] <- makeCombos(x2[i], gap = 100, preserve)
    }
    res <- do.call(rbind, res)
    x3 <- convert(res)
  }
  x1 <- convert(x1)
  xnames <- c(xnames, 'drugname.A')
  x4 <- x1[,..xnames]
  if(!is.null(x3)) {
    x4 <- rbind(x4, x3[,..xnames])
  }
  setnames(x4, 'drugname.A', 'drugname_start')
  x4 <- x4[order(filename, drugname_start)]
  class(x4) <- 'data.frame'
  if(checkForRare) {
    rare <- findRareValues(x4, colsToExclude = c('filename','drugname_start'))
    if(nrow(rare)) {
      warning("rare values found\n")
      print(rare)
    }
  }
  x4
}

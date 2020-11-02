#' Convert Character Frequency to Numeric
#'
#' This function converts the frequency entity to numeric.
#'
#' @param x character vector of extracted frequency values
#'
#' @return numeric vector
#'
#' @examples
#' f <- stdzFreq(c('in the morning', 'four times a day', 'with meals'))
#' freqNum(f)
#' @export

freqNum <- function(x) {
  x[x == 'qod'] <- '0.5'
  x[x %in% c('daily','am','pm','noon')] <- '1'
  x[x == 'bid'] <- '2'
  x[x == 'tid'] <- '3'
  x[x == 'qid'] <- '4'
  x[x == 'weekly'] <- '0.14' # 1/7
  x <- sub("^([0-9.]+)x$", "\\1", x)
  ix <- grep("^([0-9.]+)Xweek$", x)
  if(length(ix)) {
    tmp <- sub("^([0-9.]+)Xweek$", "\\1", x[ix])
    x[ix] <- sprintf("%.2f", nowarnnum(tmp) / 7)
  }
  # could add: NXmonth, monthly, Nmonthly, Nweekly
  nowarnnum(x)
}

#' Standardize Frequency Entity
#'
#' This function standardizes the frequency entity.
#'
#' @param x character vector of extracted frequency values
#'
#' @return character vector
#'
#' @examples
#' stdzFreq(c('in the morning', 'four times a day', 'with meals'))
#' @export

stdzFreq <- function(x) {
  fv <- gsub('[[:space:]]', '', tolower(x))
  ufv <- unique(fv)
  useUnq <- length(ufv) != length(fv)
  if(useUnq) {
    mix <- match(fv, ufv)
    fv <- ufv
  }
  fv[fv == ''] <- NA
  # exclude "afternoon", possibly something like "withheld"?
  ix <- grep("with|w/|after(?!noon)|before", fv, perl = TRUE)
  if(length(ix)) {
    a <- stdzFreq(sub('^(.*)(with|w/|after|before)(.*)$', '\\1', fv[ix]))
    b0 <- sub('^(.*)(with|w/|after|before)(.*)$', '\\3', fv[ix])
    b <- stdzFreq(b0)
    c <- sub('(with|w/|after|before)', '', fv[ix])
    # special case where b0 == 'meals', b could be 2 or 3
    ix1 <- which(is.na(b) | a == b | b == 'daily' | (b0 == 'meals' & a == 'bid'))
    ix2 <- which(is.na(a) | a == 'daily')
    c[ix1] <- a[ix1]
    c[ix2] <- b[ix2]
    fv[ix] <- c
  }
  fv <- gsub("[,+.]", "", fv)
  # dictionary replacement
  fv[fv == 'onceday'] <- '1'
  fv[fv == 'everyother'] <- 'qod'
  fv <- sub("and|[io]nthe", "", fv)
  fv <- sub("prn", "", fv)
  fv <- sub("weekly$", "perweekly", fv)
  fv <- sub("on(mon|tue(s)?|wed(nes)?|thur(s)?|fri|sat(ur)?|sun)(day)?", "weekly", fv)
  fv <- sub("^once$", "", fv)
  fv <- sub("(orasdirected|iftolerated)$", "", fv)
  fv <- sub("as(directed|needed|necessary)$", "", fv)
  fv <- sub("scheduled", "", fv)
  fv <- sub("^at([a-z])", "\\1", fv)
  fv <- sub("(at|@)(morning|breakfast)", "am", fv)
  fv <- sub("(at|@)(lunch|noon)", "noon", fv)
  fv <- sub("(at|@)(evening|bedtime|dinner|supper)", "pm", fv)
  # not sure about this rule
  fv <- sub("(at|@).*", "", fv)
  fv <- sub("(each|every|during)", "", fv)
  fv <- sub("/d(ay)?", "perday", fv)
  fv <- sub("(cc|var)$", "", fv)
  fv <- sub("^q?(daily|perday|a[-]?day|onday)$", "1", fv)
  fv <- sub("([0-9]+)(h|hr|hrs|hour)$", "\\1hours", fv)
  fv <- sub("([0-9]+)(d|day)$", "\\1days", fv)
  fv <- sub("day[s]?([0-9]+)$", "d\\1", fv)
  fv <- sub("(daily|perday|aday)$", "", fv)
  fv <- sub("weeks?$", "weekly", fv)
  fv <- sub("months?$", "monthly", fv)
  fv <- sub("^daily(for|beginning).*", "1", fv)
  fv <- sub("^daytime", "1", fv)
  fv <- sub("^onam", "am", fv)
  fv <- sub("(morning|breakfast|brkfst|q([0-9]+)?am)", "am", fv)
  fv <- sub("(lunch|noon)", "noon", fv)
  fv <- sub("(evening|bedtime|q([0-9]+)?pm|q?hs|afternoon|night(ly)?|nighttime)$", "pm", fv)
  fv <- sub("dinner|supper$", "pm", fv)
  # fv <- sub("after|before", "", fv)
  fv <- sub("^(per|in)", "", fv)
  fv <- sub("(times|x$)", "", fv)
  fv <- sub("(once|one)", "1", fv)
  fv <- sub("(twice|two)", "2", fv)
  fv <- sub("(three|meals?)", "3", fv)
  fv <- sub("four", "4", fv)
  fv <- sub("five", "5", fv)
  fv <- sub("six", "6", fv)
  fv <- sub("eight", "8", fv)
  fv <- sub("twelve", "12", fv)
  fv <- sub("^bi?d([0-9])*$", "2", fv)
  fv <- sub("^tid$", "3", fv)
  fv <- sub("^qid$", "4", fv)
  fv <- sub("^1(aday|daily)(in)?([ap]m)$", "\\3", fv)
  fv <- sub("(a|per)+weekly$", "Xweek", fv)
  fv <- sub("(a|per)+monthly$", "Xmonth", fv)
  ix <- grep("[0-9]+(to|-|or)[0-9]+", fv)
  a <- sub("^(.*)([0-9]+)(to|-|or)([0-9]+).*$", "\\1", fv[ix])
  b <- as.numeric(sub(".*([0-9]+)(to|-|or)([0-9]+).*", "\\1", fv[ix]))
  c <- as.numeric(sub(".*([0-9]+)(to|-|or)([0-9]+).*", "\\3", fv[ix]))
  d <- sub("^.*([0-9]+)(to|-|or)([0-9]+)(.*)$", "\\4", fv[ix])
  fv[ix] <- sprintf("%s%.2f%s", a, (b + c) / 2, d)
  ix <- grep("^q([0-9]+)$", fv)
  tmp <- as.numeric(sub("^q([0-9]+)$", "\\1", fv[ix]))
  fv[ix] <- ifelse(tmp > 24 | tmp %in% c(4,6,8,12,24,48), paste0(tmp,'hours'), paste0('d',tmp))
  fv <- sub("^q", "", fv)
  fv <- sub("^w(ee)?k$", "weekly", fv)
  fv <- sub(".*other.*day", "od", fv)
  ix <- grep("^([0-9.]+)days$", fv)
  fv[ix] <- sprintf("%.2f", 1 / as.numeric(sub("([0-9.]+)days", "\\1", fv[ix])))
  ix <- grep("([0-9]+)hours", fv)
  fv[ix] <- sprintf("%.2f", 24 / as.numeric(sub("([0-9]+)hours", "\\1", fv[ix])))
  fv <- sub("^(d([0-9]+)?|day)$", "1", fv)
  fv <- sub("^am[/-]?q?pm$", "bid", fv)
  fv <- sub("^am[/-]?q?(noon|midday|12)[/-]?q?pm$", "tid", fv)
  fv <- sub("^am[/-]?q?(noon|midday|12)$", "bid", fv)
  fv <- sub("^(hr|hour)$", "24", fv)
  fv <- sub("^o(a|p)m([0-9])*$", "0.5", fv)
  fv <- sub("^od$", "0.5", fv)

  fv[fv %in% c("0.5", "0.50")] <- "qod"
  fv[fv %in% c("1", "1.00")] <- "daily"
  fv[fv %in% c("2", "2.00", "dailybid")] <- "bid"
  fv[fv %in% c("3", "3.00")] <- "tid"
  fv[fv %in% c("4", "4.00")] <- "qid"
  fv <- sub("^([0-9.]+)$", "\\1x", fv)
  fv <- sub("^(a|p)m([0-9])*$", "\\1m", fv)

  fv[fv == ''] <- NA
  if(useUnq) {
    fv <- fv[mix]
  }
  fv
}

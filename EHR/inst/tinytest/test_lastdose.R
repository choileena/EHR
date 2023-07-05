library(EHR)

tac_mxr <- read.csv(system.file("examples", "tac_mxr.csv", package = "EHR"))
data(tac_metadata)
data(tac_lab)

expect_error(processLastDose(mxrData = tac_mxr, noteMetaData = tac_metadata[FALSE,], labData = tac_lab))

ldd <- tac_mxr[tac_mxr$entity == 'LastDose',]
d1 <- processLastDose(mxrData = tac_mxr, noteMetaData = tac_metadata, labData = tac_lab)
expect_equal(nrow(d1), nrow(ldd))
ans <- c('2008-06-25 20:30', '2008-06-25 20:42', '2008-12-15 22:30')
expect_equal(format(d1$lastdose, '%Y-%m-%d %H:%M'), ans)

data(tac_mxr_parsed)
bld <- buildDose(tac_mxr_parsed, preserve = 'lastdose')
d2 <- addLastDose(bld, d1)
expect_equal(format(na.omit(d2$lastdose), '%Y-%m-%d %H:%M'), ans)
expect_true(inherits(bld$lastdose, 'character'))
expect_true(inherits(d2$lastdose, 'POSIXt'))
expect_equivalent(bld[,setdiff(names(bld), 'lastdose')], d2[,setdiff(names(d2), 'lastdose')])

library(EHR)

tac_mxr <- read.csv(system.file("examples", "tac_mxr.csv", package = "EHR"))
data(tac_metadata)
data(tac_lab)

expect_error(processLastDose(mxrData = tac_mxr, noteMetaData = tac_metadata[FALSE,], labData = tac_lab))

ldd <- tac_mxr[tac_mxr$entity == 'LastDose',]
d1 <- processLastDose(mxrData = tac_mxr, noteMetaData = tac_metadata, labData = tac_lab)
expect_equal(nrow(d1), nrow(ldd))
expect_equal(format(d1$lastdose, '%Y-%m-%d %H:%M'), c('2008-06-25 20:30', '2008-06-25 20:42', '2008-12-15 22:30'))

library(EHR)

# parseMedEx(medx_output)
# c('DBN','DIN','DPN','DOSE','DOSEAMT','FREQ','RUT')

mxn_output <- system.file("examples", "lam_medxn.csv", package = "EHR")
mxn_parsed <- parseMedXN(mxn_output, begText = "^ID[0-9]+_[0-9-]+_")

mxr_output <- system.file("examples", "lam_mxr.csv", package = "EHR")
mxr_parsed <- parseMedExtractR(mxr_output)

clamp_output <- system.file("examples", "clamp_out.txt", package = "EHR")
clamp_parsed <- parseCLAMP(clamp_output)

expect_true(inherits(mxn_parsed, 'data.table'))
expect_true(inherits(mxr_parsed, 'data.table'))
expect_true(inherits(clamp_parsed, 'data.table'))
expect_equal(grep('^lam', sub(':.*', '', tolower(mxn_parsed$drugname))), c(1,3,4))
expect_equal(grep('^lam|ltg', sub(':.*', '', tolower(mxr_parsed$drugname))), 1:10)
expect_equal(grep('^cyto', sub(':.*', '', tolower(clamp_parsed$drugname))), c(2,4))

coi <- c('filename','drugname','strength','dose','route','freq')
expect_equal(names(mxn_parsed)[seq(6)], coi)
expect_equal(names(mxr_parsed)[seq(6)], coi)
expect_equal(names(clamp_parsed)[seq(6)], coi)
expect_equal(names(mxn_parsed)[ncol(mxn_parsed)], 'duration')
expect_equal(names(mxr_parsed)[ncol(mxr_parsed)], 'lastdose')

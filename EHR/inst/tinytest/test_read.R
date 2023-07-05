library(EHR)

tf <- tempfile()
td1 <- paste0(tf, '.RData')
td2 <- paste0(tf, '.RDS')

f1 <- system.file("examples", "tac_mxr_out.csv", package = "EHR")
# basic CSV
o1 <- EHR:::read(f1)

# RDS data set
saveRDS(o1, file = td2)
o2 <- EHR:::read(td2)

# RData environment, warning with >1 data set
save(o1, o2, file = td1)
expect_warning(o3 <- EHR:::read(td1))

expect_equal(dim(o1), c(898,4))
expect_equal(o1, o2)
expect_equal(o1, o3)

f2 <- system.file("examples", "lampid1_2016-02-05_note4_1.txt", package = "EHR")
d2 <- scan(f2, '', sep = '\n', quiet = TRUE)
save(d2, file = td1)
# generic read, no data set
expect_error(EHR:::read(f2, 'scan', list(what = '', sep = '\n', quiet = TRUE)))
# RData environment, error with no data set
expect_error(EHR:::read(td1))

expect_error(EHR:::read('nosuchfile.csv'))

expect_equal(readTransform(f1), o1)
expect_error(readTransform(f1, rename = c(position = 'pos')))

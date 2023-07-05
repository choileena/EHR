library(EHR)

suppress <- function(x) suppressMessages(suppressWarnings(x))

set.seed(6543)

doseData <- structure(list(id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  dt = c("2020-01-01 08:00", "2020-01-02 08:00", "2020-01-02 20:00",
    "2020-01-02 20:00", "2020-01-03 08:00", "2020-01-05 08:00", "2020-01-05 08:00",
    "2020-01-05 08:00", "2020-01-05 20:00", "2020-01-06 08:00", "2020-01-06 08:00",
    "2020-01-06 20:00", "2020-01-07 20:00", "2020-01-07 20:00"),
  dose = c(30, 10, 20, 30, 10, 10, 20, 30, 10, 20, 10, 10, 20, 20),
  dur = c(3600, 7200, 1800, 3600, 3600, 7200, 1800, 3600, 7200, 1800, 1800, 1800, 1800, 3600)),
  class = "data.frame", row.names = c(NA, -14L)
)

suppress(dose <- run_MedStrIII(dose.path = doseData, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose', duration = 'dur')))

expect_error(run_MedStrIII(dose.path = doseData, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose', duration = 'dur'), req.vals = list(id = 2)))
expect_error(run_MedStrIII(dose.path = doseData, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose', duration = 'dur'), req.vals = list(joker = TRUE)))

suppress(dose1 <- run_MedStrIII(dose.path = doseData, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose', duration = 'dur'), req.vals = list(id = 1)))
expect_equivalent(dose, dose1)

# no duration
# creates dupilcate which avoids discrepancy
suppress(dose2 <- run_MedStrIII(dose.path = doseData, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose')))
expect_equivalent(dose[1:5,1:3], dose2[1:5,1:3])

doseData2 <- doseData
doseData2[4,'dose'] <- doseData2[3,'dose']
# duplicate avoids discrepancy
suppress(dose3 <- run_MedStrIII(dose.path = doseData2, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose'), conflict.window = NA))
expect_equivalent(dose2, dose3[-3,])
expect_equal(doseData2[4,'dose'], dose3[3,'dose'])

td <- tempdir()
suppress(run_MedStrIII(dose.path = doseData2, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose'), conflict.window = NA, check.path = td, drugname = 'test'))
rateCon <- read.csv(file.path(td, 'fail-rateConflict-test.csv'))
rateCon[c(2,4),'flag'] <- 'keep'
fxfh <- file.path(td, 'fix-rateConflict-test.csv')
write.csv(rateCon, file = fxfh)
# utilize fix file
suppress(dose4 <- run_MedStrIII(dose.path = doseData2, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose'), conflict.window = NA, check.path = td, drugname = 'test'))
expect_equivalent(dose3, dose4[-c(5,7),])

suppress(dose5 <- run_MedStrIII(dose.path = doseData2[c(1:5,7),], dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose'), conflict.window = NA))
expect_equivalent(dose4[seq(5),], dose5)

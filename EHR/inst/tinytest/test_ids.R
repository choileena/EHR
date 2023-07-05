library(EHR)

set.seed(99)
demo_data <- data.frame(subj_id=c(4.1,4.2,5.1,6.1),
  pat_id=c(14872,14872,24308,37143),
  gender=c(1,1,0,1),
  weight=c(34,42,28,63),
  height=c(142,148,120,167)
)

conc_data <- data.frame(subj_id=rep((4:6)+0.1,each=5),
  event=rep(1:5,times=3),
  conc.level=15*exp(-1*rep(1:5,times=3))+rnorm(15,0,0.1)
)

data <- list(demo_data, conc_data)
idcols <- list(c('subj_id', 'pat_id'), 'subj_id')
xwalk <- idCrosswalk(data, idcols, visit.id='subj_id', uniq.id='pat_id')
expect_equal(c('mod_visit','mod_id','mod_id_visit'), grep('mod', names(xwalk), value = TRUE))

demo_data_deident <- pullFakeId(demo_data, xwalk,
  firstCols = c('mod_id','mod_id_visit','mod_visit'),
  uniq.id = 'pat_id'
)
expect_false('pat_id' %in% names(demo_data_deident))

demo1 <- pullRealId(demo_data_deident, xwalk)
demo2 <- pullRealId(demo_data_deident, xwalk, remove.mod.id=TRUE)

expect_equal(demo_data, demo1[,-grep('mod', names(demo1))])
expect_equal(demo_data, demo2)

# single id
# `<<-` assign for globalenv
xw <<- idCrosswalk(list(demo_data, conc_data), list('subj_id', 'subj_id'), visit.id='subj_id', uniq.id='subj_id')
attr(xw, 'deidentified_cols') <- NULL
f1 <- pullFakeId(demo_data, xw, uniq.id = 'subj_id', orderBy = 'weight')
expect_equal(seq(nrow(f1)), order(f1$weight))

# crosswalk as option
options(pkxwalk = 'xw')
f2 <- pullRealId(f1)
expect_equivalent(f1, f2[,setdiff(names(f2), 'subj_id')])

# id discrepancy
expect_error(capture.output(idCrosswalk(data, idcols, visit.id='subj_id', uniq.id='subj_id')))

library(EHR)

set.seed(2525)
dateSeq <- seq(as.Date('2019/01/01'), as.Date('2020/01/01'), by="day")
demo <- data.frame(mod_id_visit = 1:10,
                   weight.lbs = rnorm(10,160,20),
                   age = rnorm(10, 50, 10),
                   enroll.date = sample(dateSeq, 10)
)
exclude_wt <<- function(x) x < 150
exclude_age <<- function(x) x > 60
exclude_enroll <<- function(x) x < as.Date('2019/04/01')
ind.risk <<- function(wt, age) wt>170 & age>55
suppressMessages(out1 <- run_Demo(demo, demo.columns = list(id = 'mod_id_visit'),
 toexclude = expression(exclude_wt(weight.lbs) | exclude_age(age) | exclude_enroll(enroll.date))
))

suppressMessages(out2 <- run_Demo(demo, demo.columns = list(id = 'mod_id_visit'),
  demo.mod.list = list(highrisk = expression(ind.risk(weight.lbs, age)))
))

expect_equal(demo, out1$demo)
expect_equal(which(exclude_wt(demo$weight.lbs) | exclude_age(demo$age) | exclude_enroll(demo$enroll.date)), out1$exclude)
expect_equal(ind.risk(demo$weight.lbs, demo$age), out2$demo$highrisk)

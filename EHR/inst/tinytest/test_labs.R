library(EHR)

lab_data <- data.frame(mod_id=rep(1:3,each=3),
  date=rep(c("01/12/17","05/05/18","11/28/16"),each=3),
  time=rep(c("1:30","2:30","3:30"),3),
  creat=rnorm(9,0.5,0.05)
)
dat <- run_Labs(lab_data, lab.mod.list=list(log_creat=expression(log(creat))))
expect_equal(dim(lab_data) + c(0,1), dim(dat))
expect_equivalent(log(lab_data$creat), dat$log_creat)

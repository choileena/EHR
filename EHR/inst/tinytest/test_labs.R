library(EHR)

lab_data <- data.frame(mod_id=rep(1:3,each=3),
  date=rep(c("01/12/17","05/05/18","11/28/16"),each=3),
  time=rep(c("1:30","2:30","3:30"),3),
  creat=rnorm(9,0.5,0.05)
)
dat <- run_Labs(lab_data, lab.mod.list=list(log_creat=expression(log(creat))))
expect_equal(dim(lab_data) + c(0,1), dim(dat))
expect_equivalent(log(lab_data$creat), dat$log_creat)

user <- data.frame(id = c('A', 'B'), datetime = '2025-02-25 10:00')
labs <- structure(list(
    id = c("A", "A", "A", "B", "B", "B", "B", "B", "B", "B"),
    dt = structure(c(
      1738800000, 1756684800, 1775865600, 1726790400, 1737763200,
      1741910400, 1742169600, 1750118400, 1779062400, 1782345600
      ), class = c("POSIXct", "POSIXt"), tzone = ""),
    weight = c(155L, 163L, 169L, 170L, 172L, 175L, 183L, 187L, 197L, 198L),
    a1c = c(5.93, 5.97, 5.98, 5.99, 6.04, 6.04, 6.05, 6.08, 6.16, 6.17)
  ), class = "data.frame", row.names = c(NA, -10L)
)
l1 <- add_Labs(user, list(id = 'id', datetime = 'datetime'), labs, list(id = 'id', datetime = 'dt'))
expect_equal(c(id=0, datetime=0, weight=2, a1c=2), colSums(is.na(l1)))
l2 <- add_Labs(user, list(id = 'id', datetime = 'datetime'), labs, list(id = 'id', datetime = 'dt'), labPriorWindow = 30)
expect_equal(c(id=0, datetime=0, weight=0, a1c=0), colSums(is.na(l2)))

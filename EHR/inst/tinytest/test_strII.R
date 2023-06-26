library(EHR)

erx_data <- data.frame(GRID=paste0("ID",c(1,1,2,2,2,2)),
  MED_NAME=c("fakedrug","fakedrug","fakedrug","Brandname","fakedrug","fakedrug"),
  RX_DOSE=c(1,2,1,'2 tabs',1,'1+1.5+1'),
  FREQUENCY=c(rep("bid",3),"qam","bid","brkfst,lunch,dinner"),
  ENTRY_DATE=c("2018-02-15","2018-03-14","2017-07-01","2017-07-01","2017-09-15","2017-11-01"),
  STRENGTH_AMOUNT=c("100","100","200","100mg","100","100"),
  DESCRIPTION=c("fakedrug 100 mg tablet","fakedrug 100 mg tablet",
                "fakedrug 200 mg tablet (also known as brandname)",
                "Brandname 100mg tablet", "fakedrug 100 mg tablet",
                "fakedrug 100 mg tablet")
)
d1 <- erx_data
d1$desc <- NULL
d1$dt <- d1$ENTRY_DATE

o1 <- run_MedStrII(erx_data,
  list(id = 'GRID', dose = 'RX_DOSE', freq = 'FREQUENCY', date = 'ENTRY_DATE', str = 'STRENGTH_AMOUNT', desc = 'DESCRIPTION')
)

o2 <- run_MedStrII(d1, list(id = 'GRID', dose = 'RX_DOSE', freq = 'FREQUENCY', date = 'dt', str = 'STRENGTH_AMOUNT'))

expect_equal(o1$daily.dose, c(200,400,400,200,200,350))
expect_equal(o2$daily.dose, c(200,400,400,200,200,350))

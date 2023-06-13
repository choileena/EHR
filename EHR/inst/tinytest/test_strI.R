library(EHR)

flow <- data.frame(mod_id=c(1,1,2,2,2),
                  mod_id_visit=c(46723,46723,84935,84935,84935),
                  record.date=c("07/05/2019 5:25","07/05/2019 6:01",
                                "09/04/2020 3:21", "09/04/2020 4:39",
                                "09/04/2020 5:32"),
                  Final.Weight=c(6.75,6.75,4.5,4.5,4.5),
                  Final.Rate=c(rep("1 mcg/kg/hr",2),
                                rep("0.5 mcg/kg/hr",3)),
                  Final.Units=c("3.375","6.5",
                                "2.25","2.25","2.25")
)
flow[,'Perform.Date'] <- pkdata::parse_dates(flow[,'record.date'])
flow[,'pd'] <- pkdata::parse_dates(flow[,'record.date'], tz = 'UTC')
flow[,'unit'] <- sub('.*[ ]', '', flow[,'Final.Rate'])
flow[,'rate'] <- as.numeric(sub('([0-9.]+).*', '\\1', flow[,'Final.Rate']))
# conflicting with rate 0
flow[6,] <- flow[5,]
flow[6,'mod_id'] <- 3
flow[7,] <- flow[6,]
flow[7,'rate'] <- 0
# missing weight
flow[8,] <- flow[5,]
flow[8,'mod_id'] <- 4
flow[8,'Final.Weight'] <- NA

mar <- data.frame(mod_id=rep(1,5),
                  Date=rep("2019-07-05",5),
                  Time=c("07:12","07:31","08:47","09:16","10:22"),
                  `med:mDrug`=c("Fakedrug2","Fakedrug1","Fakedrug2",
                                "Fakedrug3","Fakedrug4"),
                  `med:dosage`=c("30 mg","0.5 mcg","1 mg",
                                "20 mg","3 mcg/kg/min"),
                  `med:route`=rep("IV",5),
                  `med:given`=rep("Given",5),
                  check.names=FALSE
)
mar[,'dt'] <- as.POSIXct(paste(mar$Date, mar$Time), tz = 'UTC')
mar[,'dosage'] <- c(30,0.5,1,20,3)
mar[,'unit'] <- c('mg','mcg','mg','mg','mcg/kg/min')
# bad unit
mar[6,] <- mar[2,]
mar[6,'Time'] <- '13:31'
mar[6,'dt'] <- mar[6,'dt'] + 3600*6
mar[6,'unit'] <- 'bottles/wall'
# no unit
mar[7,] <- mar[6,]
mar[7,'Time'] <- '15:31'
mar[7,'dt'] <- mar[7,'dt'] + 3600*2
mar[7,'unit'] <- NA

medcheck <- data.frame(medname="Fakedrug1",freq=4672)

td <- tempdir()

args <- list(
  mar.path = mar,
  mar.columns = list(id = 'mod_id', datetime = c('Date','Time'),
                    dose = c('dosage','unit'), drug = 'med:mDrug', given = 'med:given'),
  flow.path = flow,
  flow.columns = list(id = 'mod_id', datetime = 'Perform.Date',
                      finalunits = 'Final.Units', unit = 'unit',
                      rate = 'rate', weight = 'Final.Weight'),
  medchk.path = medcheck,
  check.path = td,
  drugname = 'fakedrg1'
)
# UTC timezone
args1 <- args
args1$mar.columns$datetime <- 'dt'
args1$flow.columns$datetime <- 'pd'

file.copy(file.path('..','examples','fixFailFlow.csv'), file.path(td, 'fixFailFlow.csv'))
file.copy(file.path('..','examples','fixNoUnit-fakedrg1.csv'), file.path(td, 'fixNoUnit-fakedrg1.csv'))
file.copy(file.path('..','examples','fixNoWgt-fakedrg1.csv'), file.path(td, 'fixNoWgt-fakedrg1.csv'))
file.copy(file.path('..','examples','fixUnit-fakedrg1.csv'), file.path(td, 'fixUnit-fakedrg1.csv'))

suppressMessages(r1 <- do.call(run_MedStrI, args))
suppressMessages(r2 <- do.call(run_MedStrI, args1))

expect_equal(nrow(r1), 10)
expect_equivalent(r1, r2)

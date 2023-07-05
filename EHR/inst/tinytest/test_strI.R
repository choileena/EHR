library(EHR)

suppress <- function(x) suppressMessages(suppressWarnings(capture.output(x)))

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

suppress(r1 <- do.call(run_MedStrI, args))
suppress(r2 <- do.call(run_MedStrI, args1))

expect_equal(nrow(r1), 10)
expect_equivalent(r1, r2)

args2 <- args
args2$check.path <- NULL
args2$mar.path <- mar[FALSE,]
args2$flow.columns$idvisit <- 'mod_id_visit'
flow2 <- flow
flow2[2,'Perform.Date'] <- flow2[1,'Perform.Date']
flow2[2,'Final.Units'] <- 0
flow2[3:5,'mod_id_visit'] <- c(2.0,2.1,2.1)
flow2[4,'Perform.Date'] <- flow2[3,'Perform.Date']
flow2[4,'Final.Units'] <- NA
flow2[3:4,'rate'] <- NA
flow2[7,'mod_id_visit'] <- 84935.1
flow2[7,'rate'] <- 0.5
flow2[7,'Final.Units'] <- NA
flow2[9,] <- flow2[8,]
flow2[8:9,'mod_id_visit'] <- c(4.1,4.2)
flow2[9,'Final.Units'] <- 0
flow2 <- rbind(flow2, flow2[3:5,])
flow2[10:12,'mod_id'] <- 5
flow2[10:12,'mod_id_visit'] <- c(5.0,5.1,5.1)
flow2[10:12,'Final.Units'] <- c(2.25,4.5,4.5)
flow2[10:12,'rate'] <- c(0.5,1,1)
args2$flow.path <- flow2
# `mod_id_visit` isn't returned?
suppress(r3 <- do.call(run_MedStrI, args2))
expect_equivalent(flow2[c(1,3,5,8,11,12),'Final.Units'], r3[,'given.dose'])

# no flow
args3 <- args
args3$flow.path <- flow[FALSE,]
args3$check.path <- NULL
suppress(r4 <- do.call(run_MedStrI, args3))
expect_equivalent(mar[2,'dosage'], r4[,'bolus.dose'])

# no flow or mar
args3$mar.path <- mar[FALSE,]
expect_warning(do.call(run_MedStrI, args3))

library(EHR)

suppress <- function(x) suppressMessages(suppressWarnings(capture.output(x)))

set.seed(26)
# concentrations
conc_data <- data.frame(mod_id = rep(1:3,each=4),
  mod_visit = rep(c(2,1,1),each=4),
  mod_id_visit = as.numeric(paste(rep(1:3,each=4), rep(c(2,1,1),each=4), sep=".")),
  samp = rep(1:4,times=3),
  drug_calc_conc=15*exp(-1*rep(1:4,times=3))+rnorm(12,0,0.1)
)

# sample times
build_date <- function(x) format(seq(x, length.out=4, by="1 hour"), "%Y-%m-%d %H:%M")
dates <- unlist(lapply(rep(Sys.time(),3), build_date))

samp_data <- data.frame(mod_id = rep(1:3,each=4),
  mod_visit = rep(c(2,1,1),each=4),
  mod_id_visit = as.numeric(paste(rep(1:3,each=4), rep(c(2,1,1),each=4), sep=".")),
  samp = rep(1:4,times=3),
  Sample.Collection.Date.and.Time = dates
)

suppress(o1 <- run_DrugLevel(
  conc.path = conc_data,
  conc.columns = list(id = 'mod_id', idvisit = 'mod_id_visit', samplinkid = 'mod_id_event', conc = 'conc.level'),
  conc.select = c('mod_id','mod_id_visit','samp','drug_calc_conc'),
  conc.rename = c(drug_calc_conc= 'conc.level', samp='event'),
  conc.mod.list = list(mod_id_event = expression(paste(mod_id_visit, event, sep = "_"))),
  samp.path = samp_data,
  samp.columns = list(conclinkid = 'mod_id_event', datetime = 'Sample.Collection.Date.and.Time'),
  samp.mod.list = list(mod_id_event = expression(paste(mod_id_visit, samp, sep = "_"))),
  drugname = 'drugnm',
  LLOQ = 0.05
))

expect_equivalent(o1$conc.level, conc_data$drug_calc_conc)

conc_data <- conc_data[,c('mod_id','mod_id_visit','samp','drug_calc_conc')]
conc_data[,'mod_id_event'] <- paste(conc_data[,'mod_id_visit'], conc_data[,'samp'], sep = "_")
names(conc_data)[3:4] <- c('event','conc.level')
samp_data[,'mod_id_event'] <- paste(samp_data[,'mod_id_visit'], samp_data[,'samp'], sep = "_")
conc_samp_link <- match(conc_data[,'mod_id_event'], samp_data[,'mod_id_event'])
conc_date <- samp_data[conc_samp_link, 'Sample.Collection.Date.and.Time']
dt <- as.POSIXct(conc_date)
conc_data[,'mydate'] <- format(dt, '%Y-%m-%d')
conc_data[,'mytime'] <- format(dt, '%H:%M')
conc_data[3,'mytime'] <- conc_data[2,'mytime']
conc_data[4,'mytime'] <- NA

td <- tempdir()
suppress(run_DrugLevel(conc_data, conc.columns = list(
  id = 'mod_id', idvisit = 'mod_id_visit', datetime = c('mydate','mytime'), conc = 'conc.level'),
  demo.list = list(demo =  data.frame(id = c(1, 2), idv = c(1.2, 2.1), weight = c(45, 50))),
  demo.columns = list(id = 'id', idvisit = 'idv'),
  check.path = td
))
noconc <- read.csv(file.path(td, 'failMissingConcDate-.csv'))
noconc[,'datetime'] <- '2023-07-05 15:18'
fxfh <- file.path(td, 'fixMissingConcDate-.csv')
write.csv(noconc, file = fxfh)
# utilize fix file
suppress(o2 <- run_DrugLevel(conc_data, conc.columns = list(
  id = 'mod_id', idvisit = 'mod_id_visit', datetime = c('mydate','mytime'), conc = 'conc.level'),
  demo.list = list(demo =  data.frame(id = c(1, 2), idv = c(1.2, 2.1), weight = c(45, 50))),
  demo.columns = list(id = 'id', idvisit = 'idv'),
  check.path = td
))
expect_equal(nrow(o2), 8)

suppress(expect_error(run_DrugLevel(conc_data, conc.columns = list(
  id = 'mod_id', idvisit = 'mod_id_visit', datetime = c('mydate','mytime'), conc = 'conc.level'),
  demo.list = list(demo =  data.frame(id = 4, idv = 4.0, weight = 99)),
  demo.columns = list(id = 'id', idvisit = 'idv')
)))

mtimes <- as.POSIXct(seq(28800, by = 14400, length.out = 4), origin = '2005-08-26', tz = 'UTC')
e1 <- data.frame(id = 1, event = 1.1, conc = 15*exp(-1*1:4)+rnorm(4,0,0.1), dt = mtimes + round(rnorm(4, sd = 1800)))
e2 <- data.frame(id = 1, event = 1.2, conc = 16*exp(-1*1:4)+rnorm(4,0,0.1), dt = mtimes + round(rnorm(4, sd = 1800)))
e3 <- data.frame(id = 1, event = 3, conc = 17*exp(-1*1:4)+rnorm(4,0,0.1), dt = mtimes + round(rnorm(4, sd = 1800)))
e4 <- rbind(e1, e1, e2, e3)
suppress(run_DrugLevel(e4, conc.columns = list(id = 'id', datetime = 'dt', conc = 'conc', idvisit = 'event'), check.path = td))
dupconc <- read.csv(file.path(td, 'failDuplicateConc-.csv'))
dupconc[seq(2, nrow(dupconc), by=2),'flag'] <- 'drop'
fxfh <- file.path(td, 'fixDuplicateConc-.csv')
write.csv(dupconc, file = fxfh)
# utilize fix file
suppress(o4 <- run_DrugLevel(e4, conc.columns = list(id = 'id', datetime = 'dt', conc = 'conc', idvisit = 'event'), check.path = td))
expect_equivalent(e1, o4[,names(e1)])

library(EHR)
data(lam_mxr_parsed)
data(lam_metadata)

lam_build_out <- buildDose(lam_mxr_parsed, checkForRare = FALSE)
lam_collapsed <- suppressWarnings(collapseDose(lam_build_out, lam_metadata, naFreq = 'most', 'xr|er'))
expect_equal(names(lam_collapsed), c('note', 'date'))
expect_equal(dim(lam_collapsed$note), c(9, 18))
c1 <- lam_collapsed$date
d1 <- data.frame(
  filename = c('lampid1_2016-02-05_note4_1.txt','lampid1_2016-02-05_note4_1.txt','lampid1_2016-02-05_note4_1.txt','lampid1_2016-02-05_note5_1.txt','lampid2_2008-07-20_note6_1.txt','lampid2_2008-07-20_note6_1.txt','lampid2_2012-04-15_note7_1.txt'),
  drugname = c('Lamictal','Lamotrigine XR','Lamotrigine XR','ltg','lamictal','Lamictal','Lamictal'),
  strength = c(NA,'100 mg','100 mg','200 mg',NA,NA,NA),
  dose = c(NA,'3','2','1.5',NA,NA,'1'),
  route = 'orally',
  freq = c('bid','am','pm','daily','bid','bid','bid'),
  dosestr = c('300 mg',NA,NA,NA,'150 mg','200mg',NA),
  dosechange = c(NA,NA,NA,NA,NA,'Increase',NA),
  lastdose = NA_character_,
  drugname_start = c(810,954,954,442,1280,2273,141),
  dosestr.num = c(300,NA,NA,NA,150,200,NA),
  strength.num = c(NA,100,100,200,NA,NA,150),
  doseamt.num = c(NA,3.0,2.0,1.5,NA,NA,1.0),
  freq.num = c(2,1,1,1,2,2,2),
  dose.intake = c(300,300,200,300,150,200,150),
  intaketime = c(NA,'am','pm',NA,NA,NA,NA),
  dose.seq = c(NA,1,2,NA,NA,NA,NA),
  dose.daily = c(600,500,500,300,300,400,300)
)
expect_equal(c1, d1)

md <- data.frame(filename = 'file1.txt', pid = 1, date = '2020-01-01', note = 1)
t1 <- data.frame(
  filename = 'file1.txt',
  drugname = 'ltg',
  strength = c('300','300'),
  dose = c('1','1'),
  freq = c('pm','am'),
  dosestr = '',
  drugname_start = 1
)
c2 <- makeDose(t1, md, 'bid')$note
o2 <- data.frame(dose.intake = c(300,300), dose.seq = c(1,2), dose.daily = c(600,600))
expect_equal(c2[,c('dose.intake','dose.seq','dose.daily')], o2)

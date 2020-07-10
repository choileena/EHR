library(EHR)

data(lam_mxr_parsed)
b1 <- buildDose(lam_mxr_parsed, checkForRare = FALSE)
d1 <- data.frame(
  filename = c("lampid1_2016-02-05_note4_1.txt","lampid1_2016-02-05_note4_1.txt","lampid1_2016-02-05_note4_1.txt","lampid1_2016-02-05_note4_1.txt","lampid1_2016-02-05_note5_1.txt","lampid1_2016-02-05_note5_1.txt","lampid1_2016-02-05_note5_1.txt","lampid2_2008-07-20_note6_1.txt","lampid2_2008-07-20_note6_1.txt","lampid2_2008-07-20_note6_1.txt","lampid2_2012-04-15_note7_1.txt","lampid2_2012-04-15_note7_1.txt"
),
  drugname = c("Lamictal","Lamotrigine","Lamotrigine XR","Lamotrigine XR","ltg","ltg xr","ltg xr","lamotrigine","lamictal","Lamictal","lamotrigine","Lamictal"),
  strength = c(NA,"200mg","100 mg","100 mg","200 mg","100 mg","100 mg",NA,NA,NA,"150 mg",NA),
  dose = c(NA,"1.5","3","2","1.5","3","2",NA,NA,NA,NA,"1"),
  route = NA_character_,
  freq = c("BID","twice daily","morning","evening","daily","in am","in pm",NA,"q12h","BID",NA,"twice a day"),
  dosestr = c("300 mg",NA,NA,NA,NA,NA,NA,NA,"150 mg","200mg",NA,NA),
  dosechange = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"Increase",NA,NA),
  lastdose = NA_character_,
  drugname_start = c(810,847,954,954,442,465,465,1267,1280,2273,103,141)
)
expect_equivalent(b1, d1)
expect_equal(attr(b1, 'class'), attr(d1, 'class'))
expect_equal(attr(b1, 'names'), attr(d1, 'names'))
expect_equal(attr(b1, 'row.names'), attr(d1, 'row.names'))

x1 <- data.frame(x = c(rep(1,99), 2), y = LETTERS[-26], z = seq(100))
expect_equivalent(EHR:::findRareValues(x1), data.frame(var = 'x', val = '2', Freq = 1, Prop = 0.01))
expect_equal(EHR:::findRareValues(x1, 0.01, colsToExclude = 'x'), NULL)
x2 <- data.frame(x = rep(1:10, 1:10))
expect_equal(nrow(EHR:::findRareValues(x2, 0.05)), 2)
expect_equal(nrow(EHR:::findRareValues(x2, 0.25)), 5)
expect_equal(nrow(EHR:::findRareValues(x2, 0.25, 1)), 9)

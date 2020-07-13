library(EHR)
s1 <- stdzStrength(c('1.5', '1/2', '1/1/1'))
o2 <- data.frame(str = c(1, 2), freq = c('am', 'pm'))
o3 <- data.frame(str = c(1, 1, 1), freq = c('am', 'noon', 'pm'))
expect_equal(c(s1), c(1.5, 1, 1))
expect_true('addl_data' %in% names(attributes(s1)))
expect_equal(attr(s1, 'addl_data'), list(NULL, o2, o3))

s2 <- stdzStrength(c('1.5', '1/2', '1/1/1'), c('am', 'daily', NA))
expect_equal(c(s2), c(1.5, 1, 1))
expect_true('addl_data' %in% names(attributes(s2)))
expect_equal(attr(s2, 'addl_data'), list(NULL, NULL, o3))

expect_equal(stdzStrength(c('1.5', '1/2', '1/1/1'), FALSE), c(1.5, 1, 1))

s3 <- stdzStrength(c('1-2', '1-2'), c('am', NA))
o4 <- data.frame(str = c(1, 2))
expect_equal(c(s3), c(1, 1))
expect_true('addl_data' %in% names(attributes(s3)))
expect_equal(attr(s3, 'addl_data'), list(o4, o2))

s4 <- stdzStrength(c('five', 'one half', 'two hundred', '1 or 2', '5mg'))
expect_equal(s4, c(5, 1, 2, 1, 5))

expect_equal(stdzRoute(c('oral', 'po', 'subcut')), c('orally', 'orally', 'sq'))
r1 <- c('skin', '', 'intravenous', 'mouth', 'subcut', NA, 'mouth', 'orally')
expect_equal(stdzRoute(r1), c('transdermal', NA, 'iv', 'orally', 'sq', NA, 'orally', 'orally'))

f <- stdzFreq(c('in the morning', 'four times a day', 'with meals', 'in the afternoon'))
expect_equal(f, c('am', 'qid', 'tid', 'pm'))
expect_equal(freqNum(f), c(1, 4, 3, 1))

f1 <- c('daily with meals', 'bid with meals', 'once', '@breakfast', 'once per day', 'every twelve hours', 'ampm')
expect_equal(stdzFreq(f1), c('tid', 'bid', NA, 'am', 'daily', 'bid', 'bid'))

f2 <- c('3 times a week', 'four per month', '2-3', 'every 8 hours', 'every 30 hours', 'every other day', 'q4hours')
expect_equal(stdzFreq(f2), c('3Xweek', '4Xmonth', '2.50x', 'tid', '0.80x', 'qod', '6.00x'))

expect_equal(freqNum(c('qod', 'noon', 'tid', 'weekly', 'daily', 'bid', '5x', '3.5Xweek')), c(0.5, 1, 3, 0.14, 1, 2, 5, 0.5))

expect_equal(stdzDuration(c('1 month', 'three days', 'two-weeks')), c('1 month(s)', '3 day(s)', '2 week(s)'))
expect_equal(stdzDuration(c('ten hours', 'one year', 'a week')), c('10 hour(s)', '1 year(s)', '1 week(s)'))

expect_equal(stdzDose(c('one tablet', '1/2 pill', '1-3 tabs')), c(1, 0.5, 2))
expect_equal(stdzDose(c('half capsule', '1-1/2', '1-2')), c(0.5, 1.5, 1.5))

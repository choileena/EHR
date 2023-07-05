library(EHR)

tac_fn <- list(system.file("examples", "tacpid1_2008-06-26_note1_1.txt", package = "EHR"),
  system.file("examples", "tacpid1_2008-06-26_note2_1.txt", package = "EHR"),
  system.file("examples", "tacpid1_2008-12-16_note3_1.txt", package = "EHR")
)

o <- suppressMessages(extractMed(tac_fn,
  drugnames = c("tacrolimus", "prograf", "tac", "tacro", "fk", "fk506"),
  drgunit = "mg",
  windowlength = 60,
  max_edit_dist = 2,
  lastdose = TRUE,
  batchsize = 10,
  progress = FALSE
))

expect_equal(nrow(o), 30)
expect_equal(sum(o$entity == 'DrugName'), 7)
odc <- o[o$entity == 'DoseChange',]
expect_equal(odc$expr, 'decrease')
expect_equal(odc$pos, '2170:2178')

# non-character note - better example would be database connection
expect_error(extractMed(1))

library(EHR)

data(dataPheWAS)
fit <- Logistf(X264.3 ~ exposure + age + race + gender, data = dd)

expect_true(inherits(fit, 'logistf'))
expect_equivalent(c(-4.23, 1.59, -0.017, 0.105, -0.03), fit$coefficients, tolerance = 1e-2)

f2 <- Logistf(X264.3 ~ exposure + age + race + gender + 0, data = dd, firth = FALSE)
expect_equal(length(f2$terms), 4)

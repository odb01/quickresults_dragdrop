# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

predictors <- c("disp", "hp", "drat", "cyl")
M1 <- c("wt", "qsec")
results <- run_linreg(imp_mtcars, predictors, "mpg", M1)

test_that("is the dataframe the correct format?", {
  expect_equal(ncol(results), 9)
})

test_that("is the first column xs?", {
  expect_equal(colnames(results)[1], 'xs')
})


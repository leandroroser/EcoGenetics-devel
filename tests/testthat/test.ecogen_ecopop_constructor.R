context("Test ecogen-ecopop constructors")


data(eco.test)


test_that("test that ecogen constructor works ok", {
  skip_on_cran()
  out <- new("ecogen")
  expect_that(out, is_a("ecogen"))
  expect_true(all(dim(out[["P"]]) == c(0, 0)))
  expect_true(all(dim(out[["G"]]) == c(0, 0)))
  expect_true(all(dim(out[["A"]]) == c(0, 0)))
  expect_true(all(dim(out[["E"]]) == c(0, 0)))
  expect_true(all(dim(out[["S"]]) == c(0, 0)))
  expect_true(all(dim(out[["C"]]) == c(0, 0)))
  expect_null(out[["OUT"]])
})

test_that("test that ecogen constructor works ok", {
  skip_on_cran()
  out <- new("ecopop")
  expect_that(out, is_a("ecopop"))
  expect_true(all(dim(out[["P"]]) == c(0, 0)))
  expect_true(all(dim(out[["AF"]]) == c(0, 0)))
  expect_true(all(dim(out[["E"]]) == c(0, 0)))
  expect_that(length(out[["S"]]), equals(0))
  expect_true(all(dim(out[["C"]]) == c(0, 0)))
})

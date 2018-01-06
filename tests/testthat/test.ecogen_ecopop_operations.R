 context("Test ecogen-ecopop operations")

data(eco.test)
data(eco3)

test_that("eco.cbind works ok", {
  skip_on_cran()
  eco.example <- eco.cbind(eco,eco,"ALL")
  expect_true(all(dim(eco.example[["XY"]]) == c(225, 2)))
  expect_true(all(dim(eco.example[["P"]]) == c(225, 16)))
  expect_true(all(dim(eco.example[["G"]]) == c(225, 20)))
  expect_true(all(dim(eco.example[["A"]]) == c(225, 80)))
  expect_true(all(dim(eco.example[["E"]]) == c(225, 12)))
  expect_true(all(dim(eco.example[["S"]]) == c(225, 2)))
  expect_true(all(dim(eco.example[["C"]]) == c(0, 0)))
  expect_null(eco.example[["OUT"]])
})


test_that("eco.split & eco.rbind work ok", {
  skip_on_cran()
  obj <- eco.split(eco3, "structure", asList = TRUE)
  obj_bind <- eco.rbind(obj)
  expect_that(length(obj), equals(3))
  expect_that(obj, is_a("ecolist"))
  expect_true(all(dim(obj_bind[["XY"]]) == c(173, 2)))
  expect_true(all(dim(obj_bind[["P"]]) == c(173, 8)))
  expect_true(all(dim(obj_bind[["G"]]) == c(173, 8)))
  expect_true(all(dim(obj_bind[["A"]]) == c(173, 53)))
  expect_true(all(dim(obj_bind[["E"]]) == c(173, 8)))
  expect_true(all(dim(obj_bind[["S"]]) == c(173, 1)))
  expect_true(all(dim(obj_bind[["C"]]) == c(0, 0)))
  expect_that(obj_bind, is_a("ecogen"))
  expect_error(eco.rbind(eco, eco), "Duplicated row names found")
})


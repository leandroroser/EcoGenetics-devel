context("Test ecogen-ecopop accessors")

data(eco.test)

test_that("test ecogen get using brackets", {
  skip_on_cran()
  expect_true(all(dim(eco[["XY"]]) == c(225, 2)))
  expect_true(all(dim(eco[["P"]]) == c(225, 8)))
  expect_true(all(dim(eco[["G"]]) == c(225, 10)))
  expect_true(all(dim(eco[["A"]]) == c(225, 40)))
  expect_true(all(dim(eco[["E"]]) == c(225, 6)))
  expect_true(all(dim(eco[["S"]]) == c(225, 1)))
  expect_true(all(dim(eco[["C"]]) == c(0, 0)))
})

test_that("test ecogen get using ecoslot notation", {
  skip_on_cran()
  expect_true(all(dim(ecoslot.XY(eco)) == c(225, 2)))
  expect_true(all(dim(ecoslot.P(eco)) == c(225, 8)))
  expect_true(all(dim(ecoslot.G(eco)) == c(225, 10)))
  expect_true(all(dim(ecoslot.A(eco)) == c(225, 40)))
  expect_true(all(dim(ecoslot.E(eco)) == c(225, 6)))
  expect_true(all(dim(ecoslot.S(eco)) == c(225, 1)))
  expect_true(all(dim(ecoslot.C(eco)) == c(0, 0)))
  expect_true(length(ecoslot.OUT(eco)) == 0)
})


test_that("test ecogen set", {
  skip_on_cran()
  new_eco <- ecogen()
  expect_true(all(dim(new_eco[["P"]] <- eco@P) == c(225, 8)))
  expect_true(all(dim(new_eco[["G", order.G=TRUE]] <- eco@G) == c(225, 10)))
  expect_that(new_eco[["A"]] <- matrix(0),  throws_error("slots can not be directly replaced"))
  expect_true(all(dim(new_eco[["E"]] <- eco@E) == c(225, 6)))
  expect_true(all(dim(new_eco[["S"]] <- eco@S) == c(225, 1)))
})

test_that("test ecopop get", {
  skip_on_cran()
  expect_true(all(dim(my_ecopop[["P"]]) == c(4, 8)))
  expect_true(all(dim(my_ecopop[["AF"]]) == c(4, 40)))
  expect_true(all(dim(my_ecopop[["E"]]) == c(4, 8)))
  expect_true(all(dim(my_ecopop[["S"]]) == c(4,2)))
  expect_true(all(dim(my_ecopop[["C"]]) == c(0, 0)))
})


test_that("test ecopop set", {
  skip_on_cran()
  new_eco <- ecopop(ploidy = 2)
  expect_true(all(dim(new_eco[["P"]] <- my_ecopop@P) == c(4, 8)))
  expect_true(all(dim(new_eco[["AF"]] <- my_ecopop@AF) == c(4, 40)))
  expect_true(all(dim(new_eco[["E"]] <- my_ecopop@E) == c(4, 8)))
  expect_true(all(dim(new_eco[["S"]] <- my_ecopop@S) == c(4, 2)))
  expect_true(all(dim(new_eco[["C"]] <- my_ecopop@C) == c(0, 0)))
})



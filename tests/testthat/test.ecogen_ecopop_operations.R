 context("Test ecogen-ecopop operations")

data(eco.test)
data(eco3)

test_that("eco.cbind works", {
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


test_that("eco.split & eco.rbind work", {
  skip_on_cran()
  obj <- eco.split(eco3, "structure", asList = TRUE)
  obj_bind <- eco.rbind(obj)
  expect_that(length(obj), equals(3))
  expect_that(obj, is_a("ecolist"))
  expect_true(all(dim(obj_bind[["XY"]]) == c(173, 2)))
  expect_true(all(dim(obj_bind[["P"]]) == c(173, 8)))
  expect_true(all(dim(obj_bind[["G"]]) == c(173, 8)))
  expect_true(all(dim(obj_bind[["A"]]) == c(173, 53)))
  expect_true(all(dim(obj_bind[["E"]]) == c(173, 11)))
  expect_true(all(dim(obj_bind[["S"]]) == c(173, 1)))
  expect_true(all(dim(obj_bind[["C"]]) == c(0, 0)))
  expect_that(obj_bind, is_a("ecogen"))
  expect_error(eco.rbind(eco, eco), "Duplicated row names found")
})

test_that("eco.subset works", {
  skip_on_cran()
  obj_bind <- eco.subset(eco3,"structure", 1) 
  expect_that(obj_bind, is_a("ecogen"))
  expect_true(all(dim(obj_bind[["XY"]]) == c(50, 2)))
  expect_true(all(dim(obj_bind[["P"]]) == c(50, 8)))
  expect_true(all(dim(obj_bind[["G"]]) == c(50, 8)))
  expect_true(all(dim(obj_bind[["A"]]) == c(50, 53)))
  expect_true(all(dim(obj_bind[["E"]]) == c(50, 11)))
  expect_true(all(dim(obj_bind[["S"]]) == c(50, 1)))
  expect_true(all(dim(obj_bind[["C"]]) == c(0, 0)))
  expect_error(eco.rbind(eco, eco), "Duplicated row names found")
})

test_that("eco.merge works", {
  skip_on_cran()
  eco1 <- eco[2:20]
  obj <- eco.merge(eco, eco1)
  expect_true(all(dim(obj[["XY"]]) == c(19, 2)))
  expect_true(all(dim(obj[["P"]]) == c(19, 16)))
  expect_true(all(dim(obj[["G"]]) == c(19, 20)))
  expect_true(all(dim(obj[["A"]]) == c(19, 74)))
  expect_true(all(dim(obj[["E"]]) == c(19, 12)))
  expect_true(all(dim(obj[["S"]]) == c(19, 2)))
  expect_true(all(dim(obj[["C"]]) == c(0, 0)))
  expect_error(eco.rbind(eco, eco), "Duplicated row names found")
})


test_that("slot OUT works", {
  skip_on_cran()
  variog <- eco.variogram(eco[["P"]][, 1], eco[["XY"]])
  ecoslot.OUT(eco) <- variog     
  expect_that(length(eco[["OUT"]]), equals(1))
  ee <- eco.remove(eco, variog)
  expect_that(length( ee[["OUT"]]), equals(0))
})


#' 
#' 
#' # the assignment of values can be made with the corresponding accessors,
#' # using the generic notation of EcoGenetics 
#' # (<ecoslot.> + <name of the slot> + <name of the object>).
#' # See help("EcoGenetics accessors")
#' 

#' 

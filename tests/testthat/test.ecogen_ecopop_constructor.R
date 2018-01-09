context("Test ecogen-ecopop constructors")


data(eco.test)


test_that("test that ecogen constructor works fine", {
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

test_that("test that ecogen constructor creates a new valid object from data", {
  skip_on_cran()
  my_ecogen <- ecogen(XY = coordinates, P = phenotype, G = genotype,
                E = environment, S = structure)
  
  my_ecogen2 <- ecogen()
  ecoslot.XY(my_ecogen2) <- coordinates
  ecoslot.P(my_ecogen2) <- phenotype
  ecoslot.G(my_ecogen2, order.G = TRUE) <- genotype
  ecoslot.E(my_ecogen2) <- environment
  ecoslot.E(my_ecogen2) - structure
  
  expect_that(my_ecogen, is_a("ecogen"))
  expect_true(all(dim(my_ecogen[["XY"]]) == c(225, 2)))
  expect_true(all(dim(my_ecogen[["P"]]) == c(225, 8)))
  expect_true(all(dim(my_ecogen[["G"]]) == c(225, 10)))
  expect_true(all(dim(my_ecogen[["A"]]) == c(225, 40)))
  expect_true(all(dim(my_ecogen[["E"]]) == c(225, 6)))
  expect_true(all(dim(my_ecogen[["S"]]) == c(225, 1)))
  expect_true(all(dim(my_ecogen[["C"]]) == c(0, 0)))
  expect_null(my_ecogen[["OUT"]])
  
  expect_that(my_ecogen2, is_a("ecogen"))
  expect_true(all(dim(my_ecogen2[["XY"]]) == c(225, 2)))
  expect_true(all(dim(my_ecogen2[["P"]]) == c(225, 8)))
  expect_true(all(dim(my_ecogen2[["G"]]) == c(225, 10)))
  expect_true(all(dim(my_ecogen2[["A"]]) == c(225, 40)))
  expect_true(all(dim(my_ecogen2[["E"]]) == c(225, 6)))
  expect_true(all(dim(my_ecogen2[["S"]]) == c(225, 1)))
  expect_true(all(dim(my_ecogen2[["C"]]) == c(0, 0)))
  expect_null(my_ecogen2[["OUT"]])
})


test_that("test that ecopop constructor works fine", {
  skip_on_cran()
  out <- new("ecopop")
  expect_that(out, is_a("ecopop"))
  expect_true(all(dim(out[["P"]]) == c(0, 0)))
  expect_true(all(dim(out[["AF"]]) == c(0, 0)))
  expect_true(all(dim(out[["E"]]) == c(0, 0)))
  expect_that(length(out[["S"]]), equals(0))
  expect_true(all(dim(out[["C"]]) == c(0, 0)))
})

test_that("ecogen2ecopop works fine", {
  skip_on_cran()
  my_ecopop <- ecogen2ecopop(eco, hier = "pop")

  XY_pop <- my_ecopop[["XY"]]
  AF_pop <- my_ecopop[["P"]]
  AF_pop <- my_ecopop[["AF"]]
  E_pop <- my_ecopop[["E"]]
  S_pop <- my_ecopop[["S"]]
 
  my_ecopop2 <- ecopop(XY = XY_pop, P = XY_pop, AF = AF_pop, E = E_pop, S = S_pop)
  
  my_ecopop3 <- ecopop()
  
  my_ecopop3[["XY"]] <- XY_pop
  my_ecopop3[["P"]] <- P_pop
  my_ecopop3[["AF", ploidy = 2]] <- AF_pop
  my_ecopop3[["E"]] <- E_pop
  my_ecopop3[["S"]] <- S_pop
  
  expect_that(my_ecopop, is_a("ecopop"))
  expect_true(all(dim(my_ecopop[["XY"]]) == c(4, 2)))
  expect_true(all(dim(my_ecopop[["P"]]) == c(4, 8)))
  expect_true(all(dim(my_ecopop[["AF"]]) == c(4, 40)))
  expect_true(all(dim(my_ecopop[["E"]]) == c(4, 8)))
  expect_that(length(my_ecopop[["S"]]), equals(4))
  expect_true(all(dim(my_ecopop[["C"]]) == c(0, 0)))
  
  expect_that(my_ecopop2, is_a("ecopop"))
  expect_true(all(dim(my_ecopop2[["XY"]]) == c(4, 2)))
  expect_true(all(dim(my_ecopop2[["P"]]) == c(4, 8)))
  expect_true(all(dim(my_ecopop2[["AF"]]) == c(4, 40)))
  expect_true(all(dim(my_ecopop2[["E"]]) == c(4, 8)))
  expect_that(length(my_ecopop2[["S"]]), equals(4))
  expect_true(all(dim(my_ecopop2[["C"]]) == c(0, 0)))
  
  expect_that(my_ecopop3, is_a("ecopop"))
  expect_true(all(dim(my_ecopop3[["XY"]]) == c(4, 2)))
  expect_true(all(dim(my_ecopop3[["P"]]) == c(4, 8)))
  expect_true(all(dim(my_ecopop3[["AF"]]) == c(4, 40)))
  expect_true(all(dim(my_ecopop3[["E"]]) == c(4, 8)))
  expect_that(length(my_ecopop3[["S"]]), equals(4))
  expect_true(all(dim(my_ecopop3[["C"]]) == c(0, 0)))
  
})

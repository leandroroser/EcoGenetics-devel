context("Test ecogen-ecopop importation/exportation methods")

require("hierfstat")
require("gstudio")
require("adegenet")
data(eco.test)
data(eco3)
data(nancycats)

test_that("genepop importation/exportation works", {
  skip_on_cran()
  expect_that(ecogen2genepop(eco, dir = "", outName = "infile.genepop.txt", 
                             grp = "pop"), prints_text("File written to"))
  ingpop <- genepop2ecogen("infile.genepop.txt")
  expect_true(all(dim(ingpop[["G"]]) == c(225, 10)))
  expect_true(all(dim(ingpop[["s"]]) == c(225, 1)))
  file.remove("infile.genepop.txt")
})

test_that("genind importation/exportation works", {
  skip_on_cran()
  outGenind <- ecogen2genind(eco)
  outEco <- genind2ecogen(outGenind)
  
  expect_that(outGenind, is_a("genind"))
  expect_true(all(dim(outGenind@tab) == c(225, 40)))
  expect_true(all(dim(outGenind@strata) == c(225, 1)))
  
  expect_that(outEco, is_a("ecogen"))
  expect_true(all(dim(outEco[["G"]]) == c(225, 10)))
  expect_true(all(dim(outEco[["S"]]) == c(225, 1)))
  
  expect_that(genind2ecogen(nancycats), is_a("ecogen"))
  
})


test_that("gstudio importation/exportation works", {
  skip_on_cran()
  togstudio <- ecogen2gstudio(eco, type = "codominant")
  toeco <- gstudio2ecogen(togstudio, ID = "ID", lat = "Latitude",
                          lon = "Longitude", struct = "pop")

  expect_true(class(togstudio[, 5]) == "locus")
  expect_true(all(dim(togstudio) == c(225, 14)))

  expect_that(toeco, is_a("ecogen"))
  expect_true(all(dim(toeco[["XY"]]) == c(225, 2)))
  expect_true(all(dim(toeco[["P"]]) == c(0, 0)))
  expect_true(all(dim(toeco[["G"]]) == c(225, 10)))
  expect_true(all(dim(toeco[["A"]]) == c(225, 40)))
  expect_true(all(dim(toeco[["E"]]) == c(0, 0)))
  expect_true(all(dim(toeco[["S"]]) == c(225, 1)))
  expect_true(all(dim(toeco[["C"]]) == c(0, 0)))
})

test_that("spagedi importation/exportation works", {
  skip_on_cran()
  expect_that(ecogen2spagedi(eco, dir = "", pop = "pop", ndig = 1,int=2, smax=6, 
                            outName="infile.spagedi.txt", to_numeric = TRUE), 
             prints_text("File written to"))
 
  toeco <- suppressWarnings(spagedi2ecogen("infile.spagedi.txt", sep = ""))
  
  expect_that(toeco, is_a("ecogen"))
  expect_true(all(dim(toeco[["XY"]]) == c(225, 2)))
  expect_true(all(dim(toeco[["P"]]) == c(0, 0)))
  expect_true(all(dim(toeco[["G"]]) == c(225, 10)))
  expect_true(all(dim(toeco[["A"]]) == c(225, 40)))
  expect_true(all(dim(toeco[["E"]]) == c(0, 0)))
  expect_true(all(dim(toeco[["S"]]) == c(225, 1)))
  expect_true(all(dim(toeco[["C"]]) == c(0, 0)))
  file.remove("infile.spagedi.txt")
  
})

test_that("ecogen2hierfstat works", {
  skip_on_cran()
  hiereco <- ecogen2hierfstat(eco, "pop", to_numeric = TRUE)
  mystats <- basic.stats(hiereco)
  expect_true(all(dim(mystats$Ho) == c(10, 4)))
})


test_that("genpop and ecopop interconversion works", {
  skip_on_cran()
  hiereco <- ecogen2hierfstat(eco, "pop",to_numeric = TRUE)
  my_genpop <- ecopop2genpop(my_ecopop)
  my_ecopop2 <- genpop2ecopop(my_genpop)
  
  expect_that(my_ecopop, is_a("ecopop"))
  expect_true(all(dim(my_ecopop[["XY"]]) == c(4, 2)))
  expect_true(all(dim(my_ecopop[["P"]]) == c(4, 8)))
  expect_true(all(dim(my_ecopop[["AF"]]) == c(4, 40)))
  expect_true(all(dim(my_ecopop[["E"]]) == c(4, 6)))
  expect_true(all(dim(my_ecopop[["S"]]) == c(4, 1)))
  expect_true(all(dim(my_ecopop[["C"]]) == c(0, 0)))
  
  expect_that(my_ecopop2, is_a("ecopop"))
  expect_true(all(dim(my_ecopop2[["XY"]]) == c(4, 2)))
  expect_true(all(dim(my_ecopop2[["P"]]) == c(0, 0)))
  expect_true(all(dim(my_ecopop2[["AF"]]) == c(4, 40)))
  expect_true(all(dim(my_ecopop2[["E"]]) == c(0, 0)))
  expect_true(all(dim(my_ecopop2[["S"]]) == c(4, 1)))
  expect_true(all(dim(my_ecopop2[["C"]]) == c(0, 0)))
  
  expect_that(my_genpop, is_a("genpop"))
  expect_true(all(dim(my_genpop@tab) == c(4, 40)))
})

test_that("data frames with population data can fill ecogen object", {
  skip_on_cran()

  # Add all the population data to the ecogen object
  out <- ecogen(S=eco[["S"]])
  obj  <- eco.fill_ecogen_with_df(out, "pop", c(1,2,3,4), 
                                   XY = my_ecopop[["XY"]], P = my_ecopop[["P"]], 
                                   E = my_ecopop[["E"]])
  expect_that(nrow(obj[["XY"]]), equals(225))
  expect_that(nrow(obj[["P"]]), equals(225))
  expect_that(nrow(obj[["E"]]), equals(225))
                                                
})

test_that("Population data of ecopop objects can be used to fill ecogen object", {
  skip_on_cran()
  obj <- ecogen(S = eco[["S"]])
  obj <- eco.fill_ecogen_with_ecopop(my_ecopop, obj, "pop")
  expect_that(nrow(obj[["XY"]]), equals(225))
  expect_that(nrow(obj[["P"]]), equals(225))
  expect_that(nrow(obj[["E"]]), equals(225))
})
  

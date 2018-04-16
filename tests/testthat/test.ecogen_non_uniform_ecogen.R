context("Test ecogen-ecopop constructors")

data(eco4)
my_ecogen <- ecogen(G=G, S=S, XY=XY, P = P, sep = "/")
my_ecopop <- ecogen2ecopop(my_ecogen, hier = "pop")


test_that("test that ecogen constructor works fine", {
  
  # error
  expect_error(ecogen(G=G, S=S, XY=XY, P = P, sep = ""))

  expect_that(my_ecogen, is_a("ecogen"))
  expect_true(all(dim(my_ecogen[["XY"]]) == c(173, 2)))
  expect_true(all(dim(my_ecogen[["P"]]) == c(173, 8)))
  expect_true(all(dim(my_ecogen[["G"]]) == c(173, 8)))
  expect_true(all(dim(my_ecogen[["A"]]) == c(173, 92)))
  expect_true(all(dim(my_ecogen[["E"]]) == c(0, 0)))
  expect_true(all(dim(my_ecogen[["S"]]) == c(173, 3)))
  expect_true(all(dim(my_ecogen[["C"]]) == c(0, 0)))
  expect_null(my_ecogen[["OUT"]])
})
  

test_that("ecogen2ecopop works fine", {
  skip_on_cran()
  
  XY_pop <- my_ecopop[["XY"]]
  P_pop <- my_ecopop[["P"]]
  AF_pop <- my_ecopop[["AF"]]
  E_pop <- my_ecopop[["E"]]
  S_pop <- my_ecopop[["S"]]
 
  my_ecopop2 <- ecopop(XY = XY_pop, P = P_pop, AF = AF_pop, E = E_pop, S = S_pop, ploidy = 2)
  
  my_ecopop3 <- ecopop(ploidy = 2)
  
  my_ecopop3[["XY"]] <- XY_pop
  my_ecopop3[["P"]] <- P_pop
  my_ecopop3[["AF"]] <- AF_pop
  my_ecopop3[["E"]] <- E_pop
  my_ecopop3[["S"]] <- S_pop
  
  expect_that(my_ecopop, is_a("ecopop"))
  expect_true(all(dim(my_ecopop[["XY"]]) == c(10, 2)))
  expect_true(all(dim(my_ecopop[["P"]]) == c(10, 8)))
  expect_true(all(dim(my_ecopop[["AF"]]) == c(10, 92)))
  expect_true(all(dim(my_ecopop[["E"]]) == c(0, 0)))
  expect_true(all(dim(my_ecopop[["S"]]) == c(10, 1)))
  expect_true(all(dim(my_ecopop[["C"]]) == c(0, 0)))
  
  expect_that(my_ecopop2, is_a("ecopop"))
  expect_true(all(dim(my_ecopop2[["XY"]]) == c(10, 2)))
  expect_true(all(dim(my_ecopop2[["P"]]) == c(10, 8)))
  expect_true(all(dim(my_ecopop2[["AF"]]) == c(10, 92)))
  expect_true(all(dim(my_ecopop2[["E"]]) == c(0, 0)))
  expect_true(all(dim(my_ecopop2[["S"]]) == c(10, 1)))
  expect_true(all(dim(my_ecopop2[["C"]]) == c(0, 0)))
  
  expect_that(my_ecopop3, is_a("ecopop"))
  expect_true(all(dim(my_ecopop3[["XY"]]) == c(10, 2)))
  expect_true(all(dim(my_ecopop3[["P"]]) == c(10, 8)))
  expect_true(all(dim(my_ecopop3[["AF"]]) == c(10, 92)))
  expect_true(all(dim(my_ecopop3[["E"]]) == c(0, 0)))
  expect_true(all(dim(my_ecopop3[["S"]]) == c(10, 1)))
  expect_true(all(dim(my_ecopop3[["C"]]) == c(0, 0)))
  
})

#-----------------------------------------------------------------------------
# 
# require("hierfstat")
# require("gstudio")
# require("adegenet")
# data(eco.test)
# data(eco3)
# data(nancycats)
# 
# test_that("genepop importation/exportation works", {
#   skip_on_cran()
#   expect_that(ecogen2genepop(eco4, dir = "", outName = "infile.genepop.txt", 
#                              grp = "pop"), prints_text("File written to"))
#   ingpop <- genepop2ecogen("infile.genepop.txt")
#   expect_true(all(dim(ingpop[["G"]]) == c(225, 10)))
#   expect_true(all(dim(ingpop[["s"]]) == c(225, 1)))
#   file.remove("infile.genepop.txt")
# })
# 
# test_that("genind importation/exportation works", {
#   skip_on_cran()
#   outGenind <- ecogen2genind(eco)
#   outEco <- genind2ecogen(outGenind)
#   
#   expect_that(outGenind, is_a("genind"))
#   expect_true(all(dim(outGenind@tab) == c(225, 40)))
#   expect_true(all(dim(outGenind@strata) == c(225, 1)))
#   
#   expect_that(outEco, is_a("ecogen"))
#   expect_true(all(dim(outEco[["G"]]) == c(225, 10)))
#   expect_true(all(dim(outEco[["S"]]) == c(225, 1)))
#   
#   expect_that(genind2ecogen(nancycats), is_a("ecogen"))
#   
# })
# 
# 
# test_that("gstudio importation/exportation works", {
#   skip_on_cran()
#   togstudio <- ecogen2gstudio(eco, type = "codominant")
#   toeco <- gstudio2ecogen(togstudio, ID = "ID", lat = "Latitude",
#                           lon = "Longitude", struct = "pop")
#   
#   expect_true(class(togstudio[, 5]) == "locus")
#   expect_true(all(dim(togstudio) == c(225, 14)))
#  
#   
#   expect_that(toeco, is_a("ecogen"))
#   expect_true(all(dim(toeco[["XY"]]) == c(225, 2)))
#   expect_true(all(dim(toeco[["P"]]) == c(0, 0)))
#   expect_true(all(dim(toeco[["G"]]) == c(225, 10)))
#   expect_true(all(dim(toeco[["A"]]) == c(225, 40)))
#   expect_true(all(dim(toeco[["E"]]) == c(0, 0)))
#   expect_true(all(dim(toeco[["S"]]) == c(225, 1)))
#   expect_true(all(dim(toeco[["C"]]) == c(0, 0)))
# })
# 
# test_that("spagedi importation/exportation works", {
#   skip_on_cran()
#   expect_that(ecogen2spagedi(eco, dir = "", pop = "pop", ndig = 1,int=2, smax=6, 
#                              outName="infile.spagedi.txt"), 
#               prints_text("File written to"))
#   
#   toeco <- suppressWarnings(spagedi2ecogen("infile.spagedi.txt", sep = ""))
#   
#   expect_that(toeco, is_a("ecogen"))
#   expect_true(all(dim(toeco[["XY"]]) == c(225, 2)))
#   expect_true(all(dim(toeco[["P"]]) == c(0, 0)))
#   expect_true(all(dim(toeco[["G"]]) == c(225, 10)))
#   expect_true(all(dim(toeco[["A"]]) == c(225, 40)))
#   expect_true(all(dim(toeco[["E"]]) == c(0, 0)))
#   expect_true(all(dim(toeco[["S"]]) == c(225, 1)))
#   expect_true(all(dim(toeco[["C"]]) == c(0, 0)))
#   file.remove("infile.spagedi.txt")
#   
# })
# 
# test_that("ecogen2hierfstat works", {
#   skip_on_cran()
#   hiereco <- ecogen2hierfstat(eco, "pop")
#   mystats <- basic.stats(hiereco)
#   expect_true(all(dim(mystats$Ho) == c(10, 4)))
# })
# 
# 
# test_that("genpop and ecopop interconversion works", {
#   skip_on_cran()
#   my_ecopop <- ecogen2ecopop(eco, hier = "pop")
#   my_genpop <- ecopop2genpop(my_ecopop)
#   my_ecopop2 <- genpop2ecopop(my_genpop)
#   
#   expect_that(my_ecopop, is_a("ecopop"))
#   expect_true(all(dim(my_ecopop[["XY"]]) == c(4, 2)))
#   expect_true(all(dim(my_ecopop[["P"]]) == c(4, 8)))
#   expect_true(all(dim(my_ecopop[["AF"]]) == c(4, 40)))
#   expect_true(all(dim(my_ecopop[["E"]]) == c(4, 6)))
#   expect_true(all(dim(my_ecopop[["S"]]) == c(4, 1)))
#   expect_true(all(dim(my_ecopop[["C"]]) == c(0, 0)))
#   
#   expect_that(my_ecopop2, is_a("ecopop"))
#   expect_true(all(dim(my_ecopop2[["XY"]]) == c(4, 2)))
#   expect_true(all(dim(my_ecopop2[["P"]]) == c(0, 0)))
#   expect_true(all(dim(my_ecopop2[["AF"]]) == c(4, 40)))
#   expect_true(all(dim(my_ecopop2[["E"]]) == c(0, 0)))
#   expect_true(all(dim(my_ecopop2[["S"]]) == c(4, 1)))
#   expect_true(all(dim(my_ecopop2[["C"]]) == c(0, 0)))
#   
#   expect_that(my_genpop, is_a("genpop"))
#   expect_true(all(dim(my_genpop@tab) == c(4, 40)))
# })
# 
# test_that("data frames with population data can fill ecogen object", {
#   skip_on_cran()
#   
#   # Add all the population data to the ecogen object
#   out <- ecogen(S=eco[["S"]])
#   obj  <- eco.fill_ecogen_with_df(out, "pop", c(1,2,3,4), 
#                                   XY = my_ecopop[["XY"]], P = my_ecopop[["P"]], 
#                                   E = my_ecopop[["E"]])
#   expect_that(nrow(obj[["XY"]]), equals(225))
#   expect_that(nrow(obj[["P"]]), equals(225))
#   expect_that(nrow(obj[["E"]]), equals(225))
#   
# })
# 
# test_that("Population data of ecopop objects can be used to fill ecogen object", {
#   skip_on_cran()
#   obj <- ecogen(S = eco[["S"]])
#   obj <- eco.fill_ecogen_with_ecopop(my_ecopop, obj, "pop")
#   expect_that(nrow(obj[["XY"]]), equals(225))
#   expect_that(nrow(obj[["P"]]), equals(225))
#   expect_that(nrow(obj[["E"]]), equals(225))
# })
# 
# 
# 
# 
# context("Test ecogen-ecopop basic methods")
# skip_on_cran()
# 
# data(eco.test)
# 
# test_that("ecogen basic methods", {
#   expect_true(is.ecogen(eco))
#   expect_that(length(nrow(eco)), equals(7))
#   expect_that(length(ncol(eco)), equals(7))
#   expect_that(length(dim(eco)), equals(7))
#   expect_that(length(as.list(eco)), equals(8))
#   expect_that(names(as.list(eco))[1], equals("XY"))
#   
#   names(eco) <- paste0("test", names(eco))
#   expect_true(na.omit(unique(c(rownames(eco@XY)[1], rownames(eco@P)[1], rownames(eco@G)[1], 
#                                rownames(eco@A)[1], rownames(eco@E)[1], rownames(eco@S)[1],
#                                rownames(eco@C)[1]))) == "testI.1")
# })
# 
# test_that("ecopop basic methods work", {
#   expect_true(is.ecopop(my_ecopop))
#   expect_that(length(nrow(my_ecopop)), equals(6))
#   expect_that(length(ncol(my_ecopop)), equals(6))
#   expect_that(length(dim(my_ecopop)), equals(6))
#   expect_that(length(as.list(my_ecopop)), equals(6))
#   expect_that(names(as.list(eco))[1], equals("XY"))
#   
#   names(my_ecopop) <- paste0("test", names(my_ecopop))
#   expect_true(na.omit(unique(c(rownames(my_ecopop@XY)[1], rownames(my_ecopop@P)[1],
#                                rownames(my_ecopop@AF)[1], rownames(my_ecopop@E)[1], 
#                                rownames(my_ecopop@S)[1], rownames(my_ecopop@C)[1]))) == "test1")
# })
# 
# 
# context("Test ecogen-ecopop accessors")
# 
# data(eco.test)
# 
# test_that("test ecogen get using brackets", {
#   skip_on_cran()
#   expect_true(all(dim(eco[["XY"]]) == c(225, 2)))
#   expect_true(all(dim(eco[["P"]]) == c(225, 8)))
#   expect_true(all(dim(eco[["G"]]) == c(225, 10)))
#   expect_true(all(dim(eco[["A"]]) == c(225, 40)))
#   expect_true(all(dim(eco[["E"]]) == c(225, 6)))
#   expect_true(all(dim(eco[["S"]]) == c(225, 1)))
#   expect_true(all(dim(eco[["C"]]) == c(0, 0)))
# })
# 
# test_that("test ecogen get using ecoslot notation", {
#   skip_on_cran()
#   expect_true(all(dim(ecoslot.XY(eco)) == c(225, 2)))
#   expect_true(all(dim(ecoslot.P(eco)) == c(225, 8)))
#   expect_true(all(dim(ecoslot.G(eco)) == c(225, 10)))
#   expect_true(all(dim(ecoslot.A(eco)) == c(225, 40)))
#   expect_true(all(dim(ecoslot.E(eco)) == c(225, 6)))
#   expect_true(all(dim(ecoslot.S(eco)) == c(225, 1)))
#   expect_true(all(dim(ecoslot.C(eco)) == c(0, 0)))
#   expect_true(length(ecoslot.OUT(eco)) == 0)
# })
# 
# 
# test_that("test ecogen set", {
#   skip_on_cran()
#   new_eco <- ecogen()
#   expect_true(all(dim(new_eco[["P"]] <- eco@P) == c(225, 8)))
#   expect_true(all(dim(new_eco[["G", order.G=TRUE]] <- eco@G) == c(225, 10)))
#   expect_that(new_eco[["A"]] <- matrix(0),  throws_error("slots can not be directly replaced"))
#   expect_true(all(dim(new_eco[["E"]] <- eco@E) == c(225, 6)))
#   expect_true(all(dim(new_eco[["S"]] <- eco@S) == c(225, 1)))
# })
# 
# test_that("test ecopop get", {
#   skip_on_cran()
#   expect_true(all(dim(my_ecopop[["P"]]) == c(4, 8)))
#   expect_true(all(dim(my_ecopop[["AF"]]) == c(4, 40)))
#   expect_true(all(dim(my_ecopop[["E"]]) == c(4, 8)))
#   expect_true(all(dim(my_ecopop[["S"]]) == c(4,2)))
#   expect_true(all(dim(my_ecopop[["C"]]) == c(0, 0)))
# })
# 
# 
# test_that("test ecopop set", {
#   skip_on_cran()
#   new_eco <- ecopop(ploidy = 2)
#   expect_true(all(dim(new_eco[["P"]] <- my_ecopop@P) == c(4, 8)))
#   expect_true(all(dim(new_eco[["AF"]] <- my_ecopop@AF) == c(4, 40)))
#   expect_true(all(dim(new_eco[["E"]] <- my_ecopop@E) == c(4, 8)))
#   expect_true(all(dim(new_eco[["S"]] <- my_ecopop@S) == c(4, 2)))
#   expect_true(all(dim(new_eco[["C"]] <- my_ecopop@C) == c(0, 0)))
# })
# 
# 
# context("Test ecogen-ecopop operations")
# 
# data(eco.test)
# data(eco3)
# 
# test_that("eco.cbind works", {
#   skip_on_cran()
#   eco.example <- eco.cbind(eco,eco,"ALL")
#   expect_true(all(dim(eco.example[["XY"]]) == c(225, 2)))
#   expect_true(all(dim(eco.example[["P"]]) == c(225, 16)))
#   expect_true(all(dim(eco.example[["G"]]) == c(225, 20)))
#   expect_true(all(dim(eco.example[["A"]]) == c(225, 80)))
#   expect_true(all(dim(eco.example[["E"]]) == c(225, 12)))
#   expect_true(all(dim(eco.example[["S"]]) == c(225, 2)))
#   expect_true(all(dim(eco.example[["C"]]) == c(0, 0)))
#   expect_null(eco.example[["OUT"]])
# })
# 
# 
# test_that("eco.split & eco.rbind work", {
#   skip_on_cran()
#   obj <- eco.split(eco3, "structure", asList = TRUE)
#   obj_bind <- eco.rbind(obj)
#   expect_that(length(obj), equals(3))
#   expect_that(obj, is_a("ecolist"))
#   expect_true(all(dim(obj_bind[["XY"]]) == c(173, 2)))
#   expect_true(all(dim(obj_bind[["P"]]) == c(173, 8)))
#   expect_true(all(dim(obj_bind[["G"]]) == c(173, 8)))
#   expect_true(all(dim(obj_bind[["A"]]) == c(173, 53)))
#   expect_true(all(dim(obj_bind[["E"]]) == c(173, 11)))
#   expect_true(all(dim(obj_bind[["S"]]) == c(173, 1)))
#   expect_true(all(dim(obj_bind[["C"]]) == c(0, 0)))
#   expect_that(obj_bind, is_a("ecogen"))
#   expect_error(eco.rbind(eco, eco), "Duplicated row names found")
# })
# 
# test_that("eco.subset works", {
#   skip_on_cran()
#   obj_bind <- eco.subset(eco3,"structure", 1) 
#   expect_that(obj_bind, is_a("ecogen"))
#   expect_true(all(dim(obj_bind[["XY"]]) == c(50, 2)))
#   expect_true(all(dim(obj_bind[["P"]]) == c(50, 8)))
#   expect_true(all(dim(obj_bind[["G"]]) == c(50, 8)))
#   expect_true(all(dim(obj_bind[["A"]]) == c(50, 53)))
#   expect_true(all(dim(obj_bind[["E"]]) == c(50, 11)))
#   expect_true(all(dim(obj_bind[["S"]]) == c(50, 1)))
#   expect_true(all(dim(obj_bind[["C"]]) == c(0, 0)))
#   expect_error(eco.rbind(eco, eco), "Duplicated row names found")
# })
# 
# test_that("eco.merge works", {
#   skip_on_cran()
#   eco1 <- eco[2:20]
#   obj <- eco.merge(eco, eco1)
#   expect_true(all(dim(obj[["XY"]]) == c(19, 2)))
#   expect_true(all(dim(obj[["P"]]) == c(19, 16)))
#   expect_true(all(dim(obj[["G"]]) == c(19, 20)))
#   expect_true(all(dim(obj[["A"]]) == c(19, 74)))
#   expect_true(all(dim(obj[["E"]]) == c(19, 12)))
#   expect_true(all(dim(obj[["S"]]) == c(19, 2)))
#   expect_true(all(dim(obj[["C"]]) == c(0, 0)))
#   expect_error(eco.rbind(eco, eco), "Duplicated row names found")
# })
# 
# test_that("slot OUT works", {
#   skip_on_cran()
#   variog <- eco.variogram(eco[["P"]][, 1], eco[["XY"]])
#   ecoslot.OUT(eco) <- variog     
#   expect_that(length(eco[["OUT"]]), equals(1))
#   ee <- eco.remove(eco, variog)
#   expect_that(length( ee[["OUT"]]), equals(0))
# })
# 

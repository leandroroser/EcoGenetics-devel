context("Test SA auxiliary functions")

require(adegenet)
require(igraph)
data(eco.test)
data(eco3)
data(eco2)


test_that("eco.bearing works fine", {
  skip_on_cran()
  
  con <- eco.weight(eco3[["XY"]], method = "circle", d1 = 0, d2 = 500)
  bearing_con <- eco.bearing(con, 90)
  
  W_list <- eco.lagweight(eco[["XY"]]) 
  bearing_W_list <- eco.bearing(W_list, 90)
  
  expect_true(class(bearing_con)  == "eco.weight")
  expect_true(class(bearing_W_list)  == "eco.lagweight")
})

test_that("eco.lagweight works fine", {
  skip_on_cran()
  
  ex1 <- eco.lagweight(eco[["XY"]]) 
  ex2 <- eco.lagweight(eco[["XY"]], smax=16) 
  ex3 <- eco.lagweight(eco[["XY"]], smin = 3, smax = 15)
  ex4 <- eco.lagweight(eco[["XY"]], cummulative = TRUE)
  ex5 <- eco.lagweight(eco[["XY"]], nclass = 4)
  ex6 <- eco.lagweight(eco[["XY"]], nclass = 4, smax = 15)
  ex7 <- eco.lagweight(eco[["XY"]], int = 2)
  ex8 <- eco.lagweight(eco[["XY"]], int = 2, smin = 3, smax = 15)
  ex9 <- eco.lagweight(eco[["XY"]], size = 1000)
  ex10 <- eco.lagweight(eco[["XY"]], size = 1000, smax = 15)
  ex11 <- eco.lagweight(eco[["XY"]], kmax = 3)
  ex12 <- eco.lagweight(eco[["XY"]], kmax = 3, self = TRUE)
  ex13 <- eco.lagweight(eco[["XY"]], seqvec = seq(0, 10, 2))
  expect_true(unique(unlist(lapply(parse(text=paste0("ex", 1:13)), function(x) class(eval(x))))) == "eco.lagweight")
})


test_that("eco.lagweight works fine", {
  skip_on_cran()

  ex1 <- eco.weight(eco3[["XY"]], method = "circle", d1 = 0, d2 = 500)
  ex2 <- eco.weight(eco3[["XY"]], method = "knearest", k = 10)
  ex3 <- eco.weight(eco3[["XY"]]/1000, method = "inverse", max.sd = TRUE, p = 0.1)
  ex4 <- eco.weight(eco3[["XY"]], method = "circle.inverse", d2 = 1000)
  ex5 <- eco.weight(eco3[["XY"]]/1000, method = "exponential", max.sd = TRUE, alpha = 0.1)
  ex6 <- eco.weight(eco3[["XY"]], method = "circle.exponential", d2 = 2000)

  tr <- make_tree(40, children = 3, mode = "undirected")
  weights <- as.matrix(as_adj(tr))
  myNames <- 1:nrow(weights)
  rownames(weights) <- colnames(weights) <-  myNames 
  coord <- layout.auto(tr)
  rownames(coord) <- myNames
  ex7 <- eco.weight(XY = coord, W = weights)

  temp <-chooseCN(eco3[["XY"]], type = 1, result.type = "listw", plot.nb = FALSE)
  ex8 <- eco.listw2ew(temp)
 
  
  expect_true(unique(unlist(lapply(parse(text=paste0("ex", 1:8)), function(x) class(eval(x))))) == "eco.weight")
  
  class(eco.plotWeight(ex1, type = "igraph", group = eco3[["S"]]$structure)) == "NULL"
  expect_true(class(eco.plotWeight(ex1, type = "network", bounded = TRUE, group = eco3[["S"]]$structure))[1] == "forceNetwork")
  expect_true(class(eco.plotWeight(ex1, type = "edgebundle", fontSize=8, group = eco3[["S"]]$structure))[1] == "edgebundleR")
  
  })


test_that("eco.slide.con works fine", {
  skip_on_cran()
  myMatrix <- eco2[["P"]]
  con <- eco.weight(XY = eco2[["XY"]], method = "knearest", k = 5)
  result <- eco.slide.con(myMatrix, con, function(x) mean(x, na.rm = TRUE))
  expect_true(all(dim(result) == c(900, 8)))
})

test_that("eco.slide.matrix works fine", {
  skip_on_cran()
  ras <- matrix(eco[["P"]][, 1], 15, 15)
  ras.square <- eco.slide.matrix(ras, 1, 1, mean, "square")
  expect_true(all(dim(ras.square) == c(13, 13)))
})

file.remove("Rplots.pdf")

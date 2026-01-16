context("Test ecogen-ecopop basic methods")
skip_on_cran()

data(eco.test)

test_that("ecogen basic methods", {
  expect_true(is.ecogen(eco))
  expect_equal(length(nrow(eco)), 7)
  expect_equal(length(ncol(eco)), 7)
  expect_equal(length(dim(eco)), 7)
  expect_equal(length(as.list(eco)), 8)
  expect_equal(names(as.list(eco))[1], "XY")

names(eco) <- paste0("test", names(eco))
expect_true(na.omit(unique(c(rownames(eco@XY)[1], rownames(eco@P)[1], rownames(eco@G)[1], 
                             rownames(eco@A)[1], rownames(eco@E)[1], rownames(eco@S)[1],
                             rownames(eco@C)[1]))) == "testI.1")
})

test_that("ecopop basic methods work", {
  expect_true(is.ecopop(my_ecopop))
  expect_equal(length(nrow(my_ecopop)), 6)
  expect_equal(length(ncol(my_ecopop)), 6)
  expect_equal(length(dim(my_ecopop)), 6)
  expect_equal(length(as.list(my_ecopop)), 6)
  expect_equal(names(as.list(eco))[1], "XY")
  
  names(my_ecopop) <- paste0("test", names(my_ecopop))
  expect_true(na.omit(unique(c(rownames(my_ecopop@XY)[1], rownames(my_ecopop@P)[1],
                               rownames(my_ecopop@AF)[1], rownames(my_ecopop@E)[1], 
                               rownames(my_ecopop@S)[1], rownames(my_ecopop@C)[1]))) == "test1")
})



test_that("ecopop basic methods work", {
  
  expect_true(is.locked(eco))
  
  unlocked <- ecogen(G = genotype, P = phenotype, lock.rows = FALSE)
  
  expect_true(!is.locked(unlocked))
  expect_true(!is.locked(eco.unlock(eco)))
  
  
  ecoslot.P(unlocked) <- rbind(ecoslot.P(eco), ecoslot.P(eco)) 
  
  expect_true(nrow(unlocked@P) != nrow(unlocked@G))
  expect_error(eco.lock(unlocked), "invalid class")
  
  ecoslot.P(unlocked) <- ecoslot.P(eco)
  locked <- eco.lock(unlocked) 
  expect_true(is.locked(locked))
  
})
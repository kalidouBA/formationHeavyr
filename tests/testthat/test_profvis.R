test_that("compare with profvis", {n <- 1000
  microbenchmark::microbenchmark(mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfoptim(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     times=100L)
})

source('../sample.R')


test_that("factorial produces value", {
  value<-factorial(8)
  expect_that(value, equals(362880))
  
  
})

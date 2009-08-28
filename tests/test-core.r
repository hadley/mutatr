context("Core")

test_that("set, get & clone work", {
  a <- Core$clone()
  a$set_slot("a", 1)
  a$get_local_slot("a")
  b <- a$clone()
  c <- a$clone()  
})
context("Core")

test("Set, get & clone", {
  a <- Core$clone()
  a$set_slot("a", 1)
  a$get_local_slot("a")
  b <- a$clone()
  c <- a$clone()  
})
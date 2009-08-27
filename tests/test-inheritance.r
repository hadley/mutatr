source("assert.r")

a <- Object$clone()
a$a <- 5

b <- a$clone()
c <- b$clone()

assert_identical(a$a, 5)
assert_identical(b$a, 5)
assert_identical(c$a, 5)

b$b <- 5
c$b <- 10

expect_error(a$b)
assert_identical(b$b, 5)
assert_identical(c$b, 10)


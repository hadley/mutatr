assert_identical <- function(a, b) {
  stopifnot(identical(a, b))
}
expect_error <- function(x) {
  res <- try(force(x), TRUE)
  stopifnot(inherits(res, "try-error"))
}

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


# Object doesn't have a parent
expect_error(Object$parent(), "no parent")

# Test simple linear inheritance
a <- Object$clone()
a$a <- 5

b <- a$clone()
b$a <- 10

assert_identical(b$parent()$a, 5)

a$f <- function() 1
b$f <- function() 1 + self$parent()$f()

assert_identical(b$f(), 2)

c <- b$clone()
c$f <- function() 1 + self$parent()$f()

assert_identical(c$f(), 3)


# Test circular inheritance
d <- Object$clone()
e <- Object$clone()
e$prepend_proto(d)
d$prepend_proto(e)

d$a <- 1
e$a <- 2

assert_identical(d$a, 1)
assert_identical(d$parent()$a, 2)
assert_identical(d$parent()$parent()$a, 1)
assert_identical(e$a, 2)
assert_identical(e$parent()$a, 1)
assert_identical(e$parent()$parent()$a, 2)
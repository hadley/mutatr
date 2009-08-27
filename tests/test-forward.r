# Test method forwarding

# Normally should return an error
a <- Object$clone()
expect_error(a$a)

# Test basic forwarding method
a$forward <- function(name) name
assert_identical(a$a, "a")
assert_identical(a$b, "b")

# Should return actual value if present
a$a <- 15
assert_identical(a$a, 15)

# Should be inherited
b <- a$clone()
b$d <- 5
assert_identical(b$a, 15)
assert_identical(b$b, "b")
assert_identical(b$c, "c")
assert_identical(b$d, 5)

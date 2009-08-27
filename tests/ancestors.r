source("assert.r")

# Test simple hierarchical case
a <- Object$clone()
b <- a$clone()
c <- b$clone()

assert_ancestors_are(a, list(Object))
assert_ancestors_are(b, list(a, Object))
assert_ancestors_are(c, list(b, a, Object))

# Test circular case
d <- Object$clone()
e <- Object$clone()
e$append_proto(d)
d$append_proto(e)

assert_ancestors_are(e, list(Object, d))
assert_ancestors_are(d, list(Object, e))

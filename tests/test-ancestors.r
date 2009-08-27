# Stateful interface, based around how tests are typically written in R 
# already - interface shouldn't look object oriented because that's what it
# normally is.  
#
# When run stand-alone (without global suite object), each just immediately
# returns results (as everything should work well interactively).  When run
# as part of a test suite, each assertion contributes to the global object, 
# which produces a final report.
# 
# should_be_error
# should_be_identical
# should_be_true
# 
# 
# should_be_same_as_last_time
#   default lookup default should hash of deparse + current test context
#
# Ancestor iteration:
#   Simple hierarchical case: ...F..
#   * a not identical to b
# 
#   Circular case: ...
#
test_context <- test <- function(...) {}

test_context("Ancestor iterator")

# ----------------------------------------------------------------------------
test("Simple hierarchical case")  
a <- Object$clone()
b <- a$clone()
c <- b$clone()

assert_ancestors_are(a, list(Object))
assert_ancestors_are(b, list(a, Object))
assert_ancestors_are(c, list(b, a, Object))

assert(c$has_ancestor(b))
assert(c$has_ancestor(a))
assert(c$has_ancestor(Object))
assert(!c$has_ancestor(c))


# ----------------------------------------------------------------------------
test("Circular case")
d <- Object$clone()
e <- Object$clone()
e$append_proto(d)
d$append_proto(e)

assert_ancestors_are(e, list(Object, d))
assert_ancestors_are(d, list(Object, e))

context("Ancestor iterator")

# ----------------------------------------------------------------------------
test("Simple hierarchical case", {
  a <- Object$clone()
  b <- a$clone()
  c <- b$clone()

  assert_has_ancestors(a, list(Object))
  assert_has_ancestors(b, list(a, Object))
  assert_has_ancestors(c, list(b, a, Object))

  assert(c$has_ancestor(b))
  assert(c$has_ancestor(a))
  assert(c$has_ancestor(Object))
  assert(!c$has_ancestor(c))
})

# ----------------------------------------------------------------------------
test("Circular case", {
  d <- Object$clone()
  e <- Object$clone()
  e$append_proto(d)
  d$append_proto(e)

  assert_has_ancestors(e, list(Object, d))
  assert_has_ancestors(d, list(Object, e))
})
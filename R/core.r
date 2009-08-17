# Boostrapping core - mostly implements usual R access, but uses object
# scoping.  Functions CAN NOT call other object functions because scope will
# be incorrect (functions not explicitly accessed via $.core will still have
# lexical scope)
# 
# Should implement:
#   * protos, append_proto, set_proto, get_proto
#   * has_slot, get_slot, set_slot, slot_names
#   * clone
#   * do
Core <- local({
  core <- new.env(TRUE, emptyenv())
  
  slot_names <- function() {
    ls(core)
  }
  
  has_local_slot <- function(name) {
    exists(name, core)
  }

  get_local_slot <- function(name) {
    get(name, core)
  }
  
  set_slot <- function(name, value) {
    assign(name, value, core)
  }
  
  remove_slot <- function(name) {
    rm(list = name, envir = core)
  }
  
  # Cloning a bootstrap object just produces an environment with usual
  # R environment inheritance and syntax
  clone <- function() {
    aclone <- new.env(FALSE, self)
    aclone$core <- new.env(TRUE, emptyenv())
    structure(aclone, class = "core")
  }
  
  self <- structure(environment(), class = "core")
})
parent.env(Core) <- baseenv()

# This slightly complicated code ensures that there is just one copy of the
# accessor functions stored in the bottom environment.
"$.core" <- function(x, i) {
  res <- get(i, x)
  if (is.function(res)) {
    environment(res) <- x
  }
  res
}

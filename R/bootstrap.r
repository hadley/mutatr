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
  
  # protos <- list()
  # #' Add prototype to end of inheritance chain
  # append_proto <- function(proto) {
  #   protos <<- c(protos, list(proto))    
  # }
  # 
  # #' Add prototype to start of inheritance chain
  # prepend_proto <- function(proto) {
  #   protos <<- c(list(proto), protos)
  # }
  # 
  # remove_proto <- function(proto) {
  #   protos[pos] <<- NULL
  # }
  
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
    # aclone$protos <- list(core)
    aclone$core <- new.env(TRUE, emptyenv())
    structure(aclone, class = "core")
  }
  
  self <- structure(environment(), class = "core")
})
parent.env(Core) <- baseenv()

"$.core" <- function(x, i) {
  res <- get(i, x)
  if (is.function(res)) {
    environment(res) <- x
  }
  res
}

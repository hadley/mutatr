source("ancestors.r")
source("object.r")

Object$do({
  self$protos <- list()
  
  # ' Add prototype to end of inheritance chain
  # ' @returns self
  self$append_proto <- function(proto) {
    stopifnot(is.io(proto))
    self$protos <- c(self$protos, list(proto))
    self
  }
  
  #' Add prototype to start of inheritance chain
  #' @returns self
  self$prepend_proto <- function(proto) {
    stopifnot(is.io(proto))
    self$protos <- c(list(proto), self$protos)
    self
  }
  
  self$remove_proto <- function(proto) {
    stopifnot(is.io(proto))
    pos <- unlist(lapply(self$protos, identical, proto))
    self$protos <- self$protos[!pos]
    self
  }

  self$has_ancestor <- function(proto) {
    i <- ancestor_iterator(self)
    i$get_next() # Skip self

    while(i$has_next()) {
      if (identical(i$get_next(), proto)) return(TRUE)
    }
    return(FALSE)
    
  }

  #' @params ... All arguments passed on to init method
  self$clone <- function(...) {
    aclone <- list(core(self)$clone()) 
    core(aclone)$set_slot("protos", list(self))
    aclone <- structure(aclone, class = "io")
    
    aclone$init(...) # initialise cloned object
    aclone
  }
  self$init <- function(...) {}

  self$has_slot <- function(name) {
    iter <- ancestor_iterator(self)

    while(iter$has_next()) {
      ancestor <- iter$get_next()
    
      if (ancestor$has_local_slot(name)) return(TRUE)
    }
    return(FALSE)
  }

  self$set_slot <- function(name, value) {
    settor <- paste("set", name, sep = "_")
    if (self$has_slot(settor)) {
      self$get_slot(settor)(value)
    } else {
      core(self)$set_slot(name, value)      
    }
  }
  
  self$get_slot <- function(name) {
    get_slot(self, name)
  }
  
  self$forward <- function(name) {
    stop("Field ", name, " not found in ", format(self), 
      call. = FALSE)     
  }

})

"$.io" <- function(x, i, ...) {
  get_slot(x, i)
}

get_slot <- function(obj, name, scope = obj) {
  iter <- ancestor_iterator(obj)
  while(iter$has_next()) {
    ancestor <- iter$get_next()
    
    # Check if a gettor function exists
    gettor <- paste("get", name, sep = "_")
    if (core(ancestor)$has_local_slot(gettor)) {
      res <- core(ancestor)$get_local_slot(gettor)
      res <- object_scope(res, scope)
      return(res())
    }
    
    # Otherwise just look for that slot 
    if (core(ancestor)$has_local_slot(name)) {
      res <- core(ancestor)$get_local_slot(name)
      res <- object_scope(res, scope)
      return(res)
    }
    
  }
  NULL
  
  iter <- ancestor_iterator(obj)
  while(iter$has_next()) {
    ancestor <- iter$get_next()
  
    # Otherwise check for forward method, and call it.
    if (core(ancestor)$has_local_slot("forward")) {
      res <- core(ancestor)$get_local_slot("forward")
      res <- object_scope(res, scope)
      return(res(name))
    }
  }
  
}
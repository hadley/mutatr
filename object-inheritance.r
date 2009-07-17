source("object.r")

Object$do({
  protos <- list()
  
  # ' Add prototype to end of inheritance chain
  # ' @returns self
  append_proto <- function(proto) {
    stopifnot(is.io(proto))
    self$protos <- c(self$protos, list(proto))
    self
  }
  
  #' Add prototype to start of inheritance chain
  #' @returns self
  prepend_proto <- function(proto) {
    stopifnot(is.io(proto))
    self$protos <- c(list(proto), self$protos)
    self
  }
  
  remove_proto <- function(proto) {
    stopifnot(is.io(proto))
    pos <- unlist(lapply(self$protos, identical, proto))
    self$protos <- self$protos[!pos]
    self
  }

  #' @params ... All arguments passed on to init method
  clone <- function(...) {
    aclone <- list(core(self)$clone()) 
    core(aclone)$set_slot("protos", list(self))
    aclone <- structure(aclone, class = "io")
    
    aclone$init(...) # initialise cloned object
    aclone
  }
  init <- function() {}

  has_slot <- function(name) {
    if (self$has_local_slot(name)) return(TRUE)
    
    for(proto in self$protos) {
      if (proto$has_slot(name)) return (TRUE)
    }
    
    return(FALSE)
  }

  set_slot <- function(name, value) {
    settor <- paste("set", name, sep = "_")
    if (self$has_slot(settor)) {
      self$get_slot(settor)(value)
    } else {
      core(self)$set_slot(name, value)      
    }
  }
  
  get_slot <- function(name) {
    .get_slot(self, name)
  }

})

"$.io" <- function(x, i, ...) {
  res <- .get_slot(x, i)
  
  if (is.null(res)) 
    stop("Field ", i, " not found in object ", substitute(x), call. = FALSE)    
  
  object_scope(res, x)
}

.get_slot <- function(obj, name, scope = obj) {
  # First check to see if a gettor function exists
  gettor <- paste("get", name, sep = "_")
  if (core(obj)$has_local_slot(gettor)) {
    res <- core(obj)$get_local_slot(gettor)
    res <- object_scope(res, scope)
    return(res())
  }
  
  # Deal with simple case first - the function is in the top level
  if (core(obj)$has_local_slot(name)) {
    res <- core(obj)$get_local_slot(name)
    res <- object_scope(res, scope)
    return(res)
  }

  .get_slot_in_parents(obj, name, scope)
} 

.get_slot_in_parents <- function(obj, name, scope = obj) {
  for(proto in core(obj)$get_local_slot("protos")) {
    res <- .get_slot(proto, name, scope = scope)
    if (!is.null(res)) return(res)
  }

  NULL
}

a <- Object$clone()$do({
  get_hadley <- function() runif(10)
  set_hadley <- function(value) stop("Not allowed")
})
b <- a$clone()
a$a <- 1

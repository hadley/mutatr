source("bootstrap-object.r")

Object$do({
  protos <- list()
  
  # ' Add prototype to end of inheritance chain
  # ' @returns self
  append_proto <- function(proto) {
    stopifnot(is.io(proto))
    self$protos <- c(self$protos, proto)
    self
  }
  
  #' Add prototype to start of inheritance chain
  #' @returns self
  prepend_proto <- function(proto) {
    stopifnot(is.io(proto))
    self$protos <- c(proto, self$protos)
    self
  }
  
  remove_proto <- function(proto) {
    stopifnot(is.io(proto))
    pos <- unlist(lapply(self$protos, identical, proto))
    self$protos <- self$protos[!pos]
    self
  }

  clone <- function(name = NULL) {
    aclone <- list(core(self)$clone()) 
    core(aclone)$set_slot("protos", list(self))
    aclone <- structure(aclone, class = "io")
    
    if (!is.null(name)) {
      aclone$.name <- name
    }
    aclone$init() # initialise cloned object
    aclone
  }
  init <- function() {}

  has_slot <- function(name) {
    if (self$has_local_slot(name)) return(TRUE)
    
    iancestors <- self$ancestors()
    while(iancestors$hasNext()) {
      if (iancestors$nextElem()$has_local_slot(name)) return(TRUE)
    }
    return(FALSE)
  }

})

"$.io" <- function(x, i, ...) {
  res <- get_slot(x, i)
  
  if (is.null(res)) 
    stop("Field ", i, " not found in object ", substitute(x), call. = FALSE)    
  
  object_scope(res, x)
}

get_slot <- function(obj, name) {
  # Deal with simple case first - the function is in the top level
  if (core(obj)$has_local_slot(name)) {
    res <- core(obj)$get_local_slot(name)
    return(res)
  }
  
  for(proto in core(obj)$get_local_slot("protos")) {
    res <- get_slot(proto, name)
    if (!is.null(res)) return(res)
  }

  NULL
} 

a <- Object$clone()
b <- Object$clone()
a$a <- 1

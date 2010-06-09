#' @include object.r
NULL

Object$do({
  self$protos <- list()
  
  # ' Add prototype to end of inheritance chain
  # ' @returns self
  self$append_proto <- function(proto) {
    stopifnot(is.mutatr(proto))
    self$protos <- c(self$protos, list(proto))
    self
  }
  
  #' Add prototype to start of inheritance chain
  #' @returns self
  self$prepend_proto <- function(proto) {
    stopifnot(is.mutatr(proto))
    self$protos <- c(list(proto), self$protos)
    self
  }
  
  self$remove_proto <- function(proto) {
    stopifnot(is.mutatr(proto))
    pos <- unlist(lapply(self$protos, identical, proto))
    self$protos <- self$protos[!pos]
    self
  }

  self$i_ancestors <- function() {
    ancestor_iterator(self)
  }

  self$parent <- function() {
    if (length(self$protos) == 0) {
      stop("Object has no parent")
    }
    
    if (!core(self)$has_local_slot(".is_parent")) {
      # To find the parent of an object, just create a new object with
      # the same protos, but no locally defined slots
      parent_obj <- Object$clone()
      parent_obj$protos <- self$protos
    } else {
      # To find the parent of a parent, take the first parent proto, and
      # replace with its parents
      parent <- self$protos[[1]]
      parents <- c(parent$protos, self$protos[-1])
      
      parent_obj <- Object$clone()
      parent_obj$protos <- parents
    }
    parent_obj$.is_parent <- TRUE    
    
    # Over-ride set slot so that all setting happens in the corrent context:
    # the original object
    parent_obj$.context <- get_context(self)
    parent_obj$set_slot <- function(name, value) {
      parent_obj$.context$set_slot(name, value)
    }
    
    parent_obj
  }

  self$has_ancestor <- function(proto) {
    i <- self$i_ancestors()
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
    aclone <- structure(aclone, class = "mutatr")
    
    aclone$init(...) # initialise cloned object
    aclone
  }

  # Default initialisation behaviour is to use set_slot to create all
  # passed in attributes
  self$init <- function(...) {
    attributes <- list(...)
    for(attr in names(attributes)) {
      self$set_slot(attr, attributes[[attr]])
    } 
  }

  self$has_slot <- function(name) {
    iter <- self$i_ancestors()

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

"$.mutatr" <- function(x, i, ...) {
  get_slot(x, i)
}

#' Get context.
#' Get object context for parent calls.
#'
#' @param obj object
#' @keywords internal
get_context <- function(obj) {
  if (core(obj)$has_local_slot(".context")) {
    core(obj)$get_local_slot(".context")
  } else {
    obj
  }
}

#' Get slot
#' 
#' Function that powers inheritance.  Given an object and a slot name
#' iterators through ancestors looking for slots that match that name.
#'
#' If the slot is a function, it adjusts the function scope (with 
#' \code{object_scope}) so that the self context is set correctly.
#'
#' @param obj object in which to look for slot
#' @param name name of slot to look for
#' @param scope self object to use for object context
#' @keywords internal
get_slot <- function(obj, name, scope = obj) {
  # TODO: look locally and then in cache
  # Cache should store object with correct scope 
  # Cache should be invalidated if value changes - i.e. set in parent
  # should invalidate all children caches - difficult! (but could be
  # accomplished with listeners on set_slot)  Abandoning dynamic
  # scoping would make this easier (but would then need to explicitly clone
  # when necessary to avoid mix of dynamic and static).  Another option would
  # be to only cache frozen objects.  
  
  iter <- ancestor_iterator(obj)
  
  while(iter$has_next()) {
    ancestor <- iter$get_next()
    
    # Look for that slot 
    if (core(ancestor)$has_local_slot(name)) {
      res <- core(ancestor)$get_local_slot(name)
      res <- object_scope(res, scope)
      return(res)
    }    
    
    # Otherwise if a gettor function exists
    gettor <- paste("get", name, sep = "_")
    if (core(ancestor)$has_local_slot(gettor)) {
      res <- core(ancestor)$get_local_slot(gettor)
      res <- object_scope(res, scope)
      return(res())
    }
  }
  
  # If slot not found anywhere, try looking for a forward method
  iter <- ancestor_iterator(obj)
  while(iter$has_next()) {
    ancestor <- iter$get_next()
  
    if (core(ancestor)$has_local_slot("forward")) {
      res <- core(ancestor)$get_local_slot("forward")
      res <- object_scope(res, scope)
      return(res(name))
    }
  }
}

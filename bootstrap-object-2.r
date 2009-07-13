Object <- local({
  .core <- Core$clone()
  .name <- "Object"
  
  
  has_slot <- function(name) {
    if (self$has_local_slot(name)) return(TRUE)
    
    iancestors <- self$ancestors()
    while(iancestors$hasNext()) {
      if (iancestors$nextElem()$has_local_slot(name)) return(TRUE)
    }
    return(FALSE)
  }
  
  get_slot <- function(name) {
    if (self$has_local_slot(name)) return(self$get_local_slot(name))
    
    iancestors <- self$ancestors()
    while(iancestors$hasNext()) {
      obj <- iancestors$nextElem()
      if (obj$has_local_slot(name)) return(obj$get_local_slot(name))
    }

    stop("Method ", name, " not found", .call = TRUE)    
  }
  
  
  proto <- function() {
    self$.core$self
  }
  super <- function() self$.core$protos[[1]]

  #' Add prototype to end of inheritance chain
  #' @returns self
  # append_proto <- function(proto) {
  #   stopifnot(is.io(proto))
  #   self[["core"]]$append_proto(proto[["core"]])
  #   self
  # }
  # 
  # #' Add prototype to start of inheritance chain
  # #' @returns self
  # prepend_proto <- function(proto) {
  #   stopifnot(is.io(proto))
  #   self[["core"]]$prepend_proto(proto[["core"]])
  #   self
  # }
  # 
  # remove_proto <- function(proto) {
  #   stopifnot(is.io(proto))
  #   self[["core"]]$remove_proto(proto[["core"]])
  #   self
  # }
  
  clone <- function(name = NULL) {
    # Need to short circuit usual accessor functions until we have enough
    # scaffolding in place
    aclone <- new.env(TRUE, baseenv())
    aclone$core <- core$clone()
    alcone$core$set_slot("proto", list(self))
    aclone <- as.io(aclone)
    
    if (!is.null(name)) {
      aclone$.name <- name
    }
    aclone$init() # initialise cloned object
    aclone
  }
  init <- function() {}


  
  # Same as set_slot, but raises an error if the slot doesn't already exist.


  do_string <- function(text) {
    eval(parse(text = text), self$proto(), self$proto())
  }
  
  #' @param chdir change working directory when evaluating code in file?
  do_file <- function(path, chdir = TRUE) {
    sys.source(path, self$proto(), chdir = chdir)
  }



  
  as.io(environment())
})
parent.env(Object[[1]]) <- baseenv()

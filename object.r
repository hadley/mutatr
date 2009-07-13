source("utils.r")
# Translation from IO
#   " " ->  $
#   :=  ->  <- 
#   R requires () for all function calls

Object <- local({
  .name <- "Object"
  
  proto <- function() {
    self[[1]]
  }
  super <- function() protos[[1]]

  protos <- list()
  
  #' Add prototype to end of inheritance chain
  #' @returns self
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
    pos <- sapply(self$protos, identical, proto)
    self$protos[pos] <<- NULL
    self
  }
  
  clone <- function(name = NULL) {
    # Need to short circuit usual accessor functions until we have enough
    # scaffolding in place
    env <- new.env(TRUE, baseenv())
    env$protos <- list(self)
    aclone <- as.io(env)
    if (!is.null(name)) {
      aclone$.name <- name
    }
    aclone$init() # initialise cloned object
    aclone
  }
  
  has_slot <- function(name, local = FALSE) {
    if (local) {
      exists(name, self$proto(), inherits = FALSE)
    } else {
      has_slot(name, TRUE) || sapply(self$protos(), 
        function(x) x$has_slot(name))
    }
  }
  
  get_slot <- function(name, local = FALSE) {
    if (local) {
      if (self$has_slot(name, TRUE)) {
        return(get(name, self$proto()))
      } else {
        return(NULL)        
      }
    } else {}
    # message(envname(env), ": getting ", name)
    gettor <- function(name) paste("get_", name, sep = "")

    
    # Try ancestors
    for(i in seq_along(self$protos)) {
      ancestor <- self$protos[[i]]
      if (ancestor$has_slot(name))

    # if (self$has_slot(gettor(name))) {
    #   # Look for gettor & execute if found
    #   get(gettor(i), self$proto())()
    # } else if (has_slot("forward")) {
    #   # Look for forward
    #   get("forward", self$proto())(name)
    # }
    
  }
  
  set_slot <- function(name, value) {
    assign(name, value, self$proto())    
  }
  
  remove_slot <- function(name) {
    rm(list = name, envir = self$proto())
  }
  
  # Same as set_slot, but raises an error if the slot doesn't already exist.
  update_slot <- function(name, value) {
    if (!has_slot(name)) {
      stop("Slot does not exist")
    }
    set_slot(name, value)
  }
  
  slot_names <- function() {
    ls(self$proto())
  }
  slot_summary <- function() {
    names <- setdiff(self$slot_names(), c("name", "protos"))
    descriptions <- unlist(lapply(names, function(name) {
      capture.output(str(get(name, self$proto()), max.level = 1, give.attr=F))
    }))
    descriptions <- gsub("^ +| +$", "", descriptions)
    
    out <- cbind(names, descriptions)
    rownames(out) <- rep("", nrow(out))
    noquote(out)
  }

  do <- function(expr) {
    eval(substitute(expr), self$proto())
    self
  }

  do_string <- function(text) {
    eval(parse(text = text), self$proto(), self$proto())
  }
  
  #' @param chdir change working directory when evaluating code in file?
  do_file <- function(path, chdir = TRUE) {
    sys.source(path, self$proto(), chdir = chdir)
  }

  as_string <- function(...) {
    paste(self$.name, " <", envname(self$proto()), ">", sep = "")
  }
  
  print <- function(...) {
    cat(self$as_string(...), "\n", sep = "")
  }
  
  as.io(environment())
})
parent.env(Object[[1]]) <- baseenv()

"$.io" <- function(x, i, ...) {
  name <- i
  env <- x[[1]]
  
  # First, look in own methods
  res <- NULL
  if (exists(name, env, inherits = FALSE)) {
    res <- get(name, env)      
  } else {
    # Next, look in inheritance chain
    for(i in seq_along(env$protos)) {
      proto <- env$protos[[i]]
      if (exists(name, proto[[1]], inherits = FALSE)) {
        res <- get(name, proto[[1]])
        break
      }
    }
    if (is.null(res)) stop("Could not find method ", name, call. = F)
  }
  
  # Reset the function environment to an environment that contains the self
  # object. This is bit of a hack because it will break closures - really
  # should replace environment further up stack. 
  if (is.function(res)) {
    env <- new.env(parent = parent.frame(2))
    env$self <- x
    environment(res) <- env
  }
  res
}

"$<-.io" <- function(x, i, value) {
  x$set_slot(i, value)
  x
}

print.io <- function(x, ...) {
  x$print()
}

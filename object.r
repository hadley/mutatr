# Translation from IO
#   " " ->  $
#   :=  ->  <- 
#   R requires () for all function calls

Object <- local({
  .name <- "Object"
  
  proto <- function() {
    self[[1]]
  }
  super <- function() protos[[2]]

  protos <- function() {
    lapply(envlist(self$proto()), as.io)
    # How to make this a tree, rather than a list?
  }
  
  clone <- function(name = "Object") {
    # Create new environment with this object as parent
    env <- new.env(TRUE, self$proto())
    cloned <- as.io(env)
    cloned$.name <- name
    # Run initialise
    cloned$init()
    cloned
  }
  init <- function() {}
  
  has_slot <- function(name, local = FALSE) {
    exists(name, self$proto(), inherits = !local)
  }
  
  get_slot <- function(name, self) {
    # message(envname(env), ": getting ", name)
    gettor <- function(name) paste("get_", name, sep = "")

    # if (self$has_slot(gettor(name))) {
    #   # Look for gettor & execute if found
    #   get(gettor(i), self$proto())()
    # } else if (has_slot("forward")) {
    #   # Look for forward
    #   get("forward", self$proto())(name)
    # }
    
    res <- get(name, self[[1]])
    
    # Reset the function environment to an environment that contains the self
    # object. This is bit of a hack because it will break closures - really
    # should replace environment further up stack. 
    if (is.function(res)) {
      env <- new.env(parent = parent.frame(2))
      env$self <- self
      environment(res) <- env
    }
    res
  }
  
  set_slot <- function(name, value) {
    assign(name, value, self$proto())    
  }
  
  remove_slot <- function(name) {
    set_slot(name, NULL)
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
    names <- self$slot_names()
    descriptions <- unlist(lapply(names, function(name) {
      capture.output(str(get(name, self$proto()), max.level = 1, give.attr=F))
    }))
    descriptions <- gsub("^ +| +$", "", descriptions)
    
    out <- cbind(names, descriptions)
    rownames(out) <- rep("", nrow(out))
    noquote(out)
  }

  do <- function(expr) {
    eval(substitute(expr), self$proto(), self$proto())
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
  get("get_slot", x[[1]])(i, x)
}

"$<-.io" <- function(x, i, value) {
  x$set_slot(i, value)
  x
}

print.io <- function(x, ...) {
  x$print()
}
envname <- function(env) {
  gsub("<environment: |>", "", utils::capture.output(print(env)))
}

envlist <- function(env) {
  if (is.emptyenv(env)) return()
  if (is.globalenv(env)) return(list(env))
  
  c(env, envlist(parent.env(env)))
}
is.emptyenv <- function(env) identical(env, emptyenv())
is.globalenv <- function(env) identical(env, globalenv())
is.baseend <- function(env) identical(env, baseenv())

as.io <- function(x) UseMethod("as.io")
as.io.environment <- function(x) {
  # Return unchanged if not really an io object
  if (!exists("get_slot", x))  return(x)

  structure(list(x), class = "io")
}

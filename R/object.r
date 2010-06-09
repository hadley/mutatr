#' @include core.r
#' @include utils.r
NULL

#' Core mutatr object
#' 
#' @aliases Object $.mutatr $<-.mutatr format.mutatr print.mutatr
#' @S3method format mutatr
#' @S3method print mutatr
#' @S3method "$" mutatr
#' @S3method "$<-" mutatr
#' @keywords programming
#' @export
Object <- structure(list(Core$clone()), class = "mutatr")
core(Object)$set_slot("set_slot", function(name, value) {
  core(self)$set_slot(name, value)
})

#' Add object scope to a function
#' 
#' @param res object to add object scope to (does nothing if not a function)
#' @param self self object
#' @keywords internal
object_scope <- function(res, self) {
  # Add environment to top of stack that contains the self object
  if (is.function(res)) {
    env <- new.env()
    env$self <- self
    parent.env(env) <- environment(res)
    environment(res) <- env
  }
  res
}


# Basic bootstrapping accessor - can do better once object has more methods
"$.mutatr" <- function(x, i, ...) {
  res <- core(x)$get_local_slot(i)
  object_scope(res, x)
}
"$<-.mutatr" <- function(x, i, value) {
  x$set_slot(i, value)
  x
}

# Next we create a do function that lets us define multiple functions
# simultaneously.
Object$do <- function(expr) {
  env <- new.env(parent = parent.frame())
  env$self <- self
  eval(substitute(expr), env)
  self
}

Object$do({
  self$.name <- "Object"
  
  self$do_string <- function(text) {
    env <- new.env(parent = parent.frame())
    env$self <- self
    eval(parse(text = text), env)
    self
  }
  
  #' @param chdir change working directory when evaluating code in file?
  self$do_file <- function(path, chdir = TRUE) {
    env <- new.env(parent = parent.frame())
    env$self <- self
    sys.source(path, env, chdir = chdir)
    self
  }
  
  self$remove_slot <- function(name) {
    core(self)$remove_slot(name)
  }
  
  self$slot_names <- function() {
    core(self)$slot_names()
  }
  
  self$has_local_slot <- function(name) {
    core(self)$has_local_slot(name)
  }
  self$get_local_slot <- function(name) {
    core(self)$get_local_slot(name)
  }
  
  self$update_slot <- function(name, value) {
    if (!self$has_slot(name)) {
      stop("Slot does not exist")
    }
    self$set_slot(name, value)
  }
  
  self$slot_summary <- function() {
    names <- setdiff(self$slot_names(), c("name", "protos"))
    descriptions <- unlist(lapply(names, self$slot_desc))
    descriptions <- gsub("^ +| +$", "", descriptions)
    
    out <- cbind(Name = names, Description = descriptions)
    out <- out[order(out[, 1]), ]
    rownames(out) <- rep("", nrow(out))
    noquote(out)
  }
  
  self$slot_desc <- function(name) {
    capture.output(str(self$get_local_slot(name), max.level = 1, give.attr=F))
  }

  self$as_string <- function(...) {
    paste(self$.name, " <", envname(core(self)[[1]]), ">", sep = "")
  }
  
  self$format <- function(...) {
    self$as_string(...)
  }
})


format.mutatr <- function(x, ...) {
  x$as_string(...)
}
print.mutatr <- function(x, ...) {
  cat(format(x), "\n", sep = "")
}

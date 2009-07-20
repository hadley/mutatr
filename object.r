source("utils.r")
source("bootstrap.r")

core <- function(x) x[[1]]

# Set up minimal object - just need to be add new slots
Object <- structure(list(Core$clone()), class = "io")
core(Object)$set_slot("set_slot", function(name, value) {
  core(self)$set_slot(name, value)
})

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
"$.io" <- function(x, i, ...) {
  res <- core(x)$get_local_slot(i)
  object_scope(res, x)
}
"$<-.io" <- function(x, i, value) {
  x$set_slot(i, value)
  x
}

# Next we create a do function that lets us define
# multiple functions simultaneously.  Basic strategy is to evaluate in
# temporary environment and then copy everything across with set_slot. 
Object$do <- function(expr) {
  env <- new.env(parent = globalenv())
  env$self <- self
  eval(substitute(expr), env)
  self

  # env <- new.env(parent = globalenv())
  # eval(substitute(expr), env)
  # self$load_enviro(env)
}
# Object$load_enviro <- function(env) {
#   for(name in ls(env, all = TRUE)) {
#     self$set_slot(name, get(name, env))
#   }
#   
#   self
# }


Object$do({
  self$.name <- "Object"
  
  self$do_string <- function(text) {
    env <- new.env(parent = globalenv())
    eval(parse(text = text), env)
    self$load_enviro(env)
  }
  
  #' @param chdir change working directory when evaluating code in file?
  self$do_file <- function(path, chdir = TRUE) {
    env <- new.env(parent = globalenv())
    sys.source(path, env, chdir = chdir)
    self$load_enviro(env)
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
    paste(self$.name, " <", envname(core(self)), ">", sep = "")
  }
  
  self$print <- function(...) {
    cat(self$as_string(...), "\n", sep = "")
  }
})


print.io <- function(x, ...) {
  x$print()
}

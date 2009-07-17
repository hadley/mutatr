# source("gui-load.r")
library(gWidgets)
source("object-inheritance.r")

source("gui-widget.r")
source("gui-window.r")
source("gui-button.r")
source("gui-group.r")

w <- Window$clone()$do({
  title <- "Hello"
  
  buttons <- Group$clone("v")$do({
    b1 <- Button$clone()$do(label <- "One")$
      add_handler("on_click", function(...) print("hi"))
    # self$add_spring()
    b2 <- Button$clone()$do(label <- "Two")
  })
})

w$show()
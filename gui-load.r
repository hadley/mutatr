# source("gui-load.r")
library(gWidgets)
source("object-inheritance.r")

source("gui-widget.r")
source("gui-window.r")
source("gui-button.r")



w <- Window$clone()$do({
  title <- "Hello"
  
  button <- Button$clone()$do({
    label <- "help"
  })
})

w$show()
w$button$label
w$button$label <- "Help"

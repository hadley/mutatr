# source("gui-load.r")
library(gWidgets)
source("object-inheritance.r")

source("gui-widget.r")
source("gui-window.r")
source("gui-button.r")
source("gui-group.r")


# Advantages:
#   * don't have to worry about polluting global namespace
#   * hierarchy of object names exactly matches hierarchy of gui
#   * indenting level exactly matches hierarchy of gui
# Disadvantages
#   * need to name full tree


w <- Window$clone()$do({
  self$title <- "Hello"
  
  self$buttons <- Group$clone("v")$
    add(Button$clone("One")$
      add_handler("on_click", function(...) print("hi"))
    )$
    add_spring()$
    add(Button$clone("Two"))$
    add_space(10)$
    add(Button$clone("Three"))
})
  


w2 <- Window$clone()$do({
  self$title <- "Hello"
  
  self$buttons <- Group$clone("v")$do({
    # When you don't want to access later
    self$add(Button$clone("One")$
      add_handler("on_click", function(...) print("hi"))
    )
    self$add_spring()
    # When you do want to access later
    self$two <- Button$clone("Two")
    self$add_space(10)
    self$three <- Button$clone("Three")
  })
})  

w2$show()

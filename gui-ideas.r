# Advantages:
#   * don't have to worry about polluting global namespace
#   * hierarchy of object names exactly matches hierarchy of gui
#   * indenting level exactly matches hierarchy of gui
# Disadvantages
#   * need to name full tree

w <- Window$clone()$do({
  title <- "Tour"
  
  tabs <- tabs_object_created_else_where$clone()
  
  # When you don't want to access later
  self$add(Label$clone("This is a label"))
  
  # When you do want to access later
  stop <- Button$clone()$do({
    state <- FALSE
    label <- "Pause"
    
    self$add_handler("on_push", function() self$state <- !self$state)    
  })
  
})


w$show()
w$title <- "A new title"
w$stop$click()
w$stop$state
w$stop$click()
w$stop$state




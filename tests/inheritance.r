source("../object-inheritance.r", chdir = T)
source("../examples/observer.r", chdir = T)

a <- Object$clone()
expect_error(a$signal())

a$append_proto(Observable)
a$add_listener(function() print("Hi"), "hi")
a$signal()

a$remove_proto(Observable)
expect_error(a$signal())

sup <- Observable$clone()
sup$add_listener(function() print("Wassup"), "yo")
sup$signal()

a$append_proto(sup)
a$signal()
a$remove_slot("listeners")
a$signal()
a$add_listener(function() print("Testing"), "123")
a$signal()

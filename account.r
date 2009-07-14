source("bootstrap-inheritance.r")

Account <- Object$clone()
Account$balance <- 0.0
Account$deposit <- function(v) self$balance <- self$balance + v
Account$show <- function() cat("Account balance: $", self$balance, "\n")
Account$init <- function() self$balance <- 0
 
# vs 

Account <- Object$clone()$do({
  balance <- 0.0
  deposit <- function(v) self$balance <- self$balance + v
  withdraw <- function(v) self$balance <- self$balance - v
  show <- function() cat("Account balance: $", self$balance, "\n")
  init <- function() self$balance <- 0
})

cat("Inital: ")
Account$show()
 
cat("Depositing $10\n")
Account$deposit(10.0)
 
cat("Final: ")
Account$show()

Savings <- Account$clone()$do({
  interest <- 0.05
  withdraw <- NULL
})
Savings$show()


# Closures work too :)
adder <- function(y) function(x) x + y
Account$test <- adder(100)
Account$test(10)


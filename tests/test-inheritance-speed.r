# Do we need a function cache?

time_n <- function(code, n = 1e3) {
  quoted <- substitute(code)
  time <- system.time(for(i in seq_len(n)) {
    eval(quoted)
  })
  
  unname(time[3] / n)
}


a <- Object$clone()
a$a <- 1

b <- a$clone()
c <- b$clone()
d <- c$clone()
e <- d$clone()

# Currently slow even for top level.  Considerable ecrease in 
# speed as hierarchy deepens, but not a huge difference
times <- c( 
 a = time_n(a$a, 100), # 3500 / s
 b = time_n(b$a, 100),
 c = time_n(c$a, 100), # 1900 / s
 d = time_n(d$a, 100),
 e = time_n(e$a, 100)  # 800 / s
)

1 / time_n(a$a, 100)
1 / time_n(get_slot(a, "a"), 100)
1 / time_n(core(a)$get_local_slot("a"), 1000)
1 / time_n(core(a)$has_local_slot("a"), 1000)

# Large amount of time spent creating ancestor iterator, which isn't 
# even necessary in this case
1 / time_n(ancestor_iterator(a)$get_next())

# Aim for 10th of speed of raw variable access - i.e. ~ 20,000 / s

x <- 1
time_n(x)
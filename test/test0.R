# test
library(devtools)
load_all(".")

t0 <- looptimer()

t1 <- looptimer(t0)

t2 <- looptimer(t0, n=5)

t3 <- looptimer(t0, n=5, i=2)

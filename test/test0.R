# test
library(devtools)
load_all(".")

t0 <- looptimer()

t1 <- looptimer(t0)

t2 <- looptimer(n=5)

t3 <- looptimer(t0, n=5, i=2)

t2 <- looptimer(t2)

summary(t2)

t4 <- looptimer(n = 10, endline = "  \r")
for(i in 1:10) 
  print(t4 <- looptimer(t4))
summary(t4)



t4 <- looptimer(n = 30, endline = "  \r", printevery=3)
for(i in 1:30) {
  Sys.sleep(.5)
  print(t4 <- looptimer(t4))
}
summary(t4)

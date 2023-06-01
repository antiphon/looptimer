# cat or message?

library(devtools)
load_all(".")

t4 <- looptimer(n = 30, endline = "  \r", printevery=3, use_msg = TRUE)
for(i in 1:30) {
  Sys.sleep(.05)
  print(t4 <- looptimer(t4))
}
summary(t4)

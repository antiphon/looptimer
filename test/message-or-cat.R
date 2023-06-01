# cat or message?

library(devtools)
load_all(".")



t4 <- looptimer(n = 30, endline = "  \r", printevery=3, use_msg = TRUE)
print(t4 <- looptimer(t4))

summary(t4)

suppressMessages(print(t4 <- looptimer(t4)))
suppressMessages(summary(t4))

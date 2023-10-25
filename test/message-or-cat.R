# cat or message?

library(devtools)
load_all(".")

t4 <- looptimer(n = 30, endline = "  \r", printevery=1, use_msg = TRUE)

# Should print
print(t4 <- looptimer(t4))
summary(t4)

# but not now
suppressMessages(print(t4 <- looptimer(t4)))
suppressMessages(summary(t4))

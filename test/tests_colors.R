# test colors

devtools::load_all()

t0 <- looptimer(n = n <- 10, fg = 25, bg = 230)

for(i in 1:n) print(t0 <- looptimer(t0))

summary(t0)
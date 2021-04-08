# test colors

devtools::load_all()

t0 <- looptimer(n = n <- 10, color = c(255,20))

for(i in 1:n) print(t0 <- looptimer(t0))
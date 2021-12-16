source("/Users/simonperschel/Dropbox/Bachelorarbeit Mathe/Scripts/algorithms.R")
library(itsmr)
set.seed(2021)

# Generiere Daten
a <- specify(ma = c(1/2))
x <- sim(a, 50)
gam <- c(3/2, 1/2, rep(0, 10))

# Visualisiere Daten
plot(x, type = "l")

# Korrelationen betrachtn
itsmr::acvf(x, 10)
n.lags <- c(2, 5)

# Parameterberechnen
params.dl <- durbin.levinson(gam = gam, n.lags = 5)

for(n in n.lags){
  dl.pred <- predict.dl(x, params = params.dl$d, n.lags = n)
  v <- params.dl$v[n+1]
  p <- paste("/Users/simonperschel/Dropbox/Bachelorarbeit Mathe/Arbeit/3. Algorithmen/Images/test_alg_", n, ".pdf", sep = "")
  pdf(p)
  plot(x, type = "l", main = paste("n = ", n, ", v = ", round(v, 4), ", mse = ", round(dl.pred$mse, 2)))
  points(dl.pred$pred, type = "l", col = "red", add = T)
  dev.off()
}





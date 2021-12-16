library('latex2exp')

set.seed(243)
n <- 100
mean <- -1/2

y <- c(0, rnorm(mean = mean, n = n))
x <- cumsum(y)

t <- 0:n
model <- lm(x~t)

pdf("/Users/simonperschel/Dropbox/Bachelorarbeit Mathe/Arbeit/2.Grundlagen/Images/CumSum.pdf")
plot(x, type = "l", xlab = "t", ylab = TeX("$x_{t}$"), main = "Realisierung des Prozesses mit wahrem und geschätztem Trend")
lines(0:(n+1), -1/2 * 0:(n+1), pch = 18, col = "red", lty = 2)
abline(model, add = T, col = "green", add = T)
legend(1, -20, legend = c(TeX("$x_{t}$"), TeX("$m_{t}$"), TeX("$\\hat{m}_{t}$")), col=c("black", "red", "green"), lty=1:2, cex=1)
dev.off()

xp <- x - predict(model)
pdf("/Users/simonperschel/Dropbox/Bachelorarbeit Mathe/Arbeit/2.Grundlagen/Images/CumSum_Stationaer_model.pdf")
plot(xp, type = "l", xlab = "t", ylab = TeX("$x_{t}$"), main = "Zeitreihe nach Abziehen des geschätzten Trends")
legend(1, 4, legend = c(TeX("$x_{t} - \\hat{m}_{t}$")), col=c("black"), lty=1:2, cex=1)
dev.off()

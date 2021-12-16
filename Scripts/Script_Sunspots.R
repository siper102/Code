##### SETUP #####
library(itsmr)
library('latex2exp')

clear.date <- function(x){unlist(strsplit(x, ".", 2))[1]}

wd <- "/Users/simonperschel/Dropbox/Bachelorarbeit Mathe/Scripts/"
setwd(wd)
source("algorithms.R")

##### LADE DATEN #####
url <- "https://wwwbis.sidc.be/silso/INFO/snytotcsv.php"
data.all <- read.csv2(url, header = F,
              col.names = c("date", "value", ".", ".", "."))[,1:2]

data.all$date <- unlist(lapply(data.all$date, clear.date))
data.all$value <- as.numeric(data.all$value)

data.sunspots <- subset(data.all, date %in% 1770:1869)

##### Daten zentrieren #####
mean.values <- mean(data.sunspots$value)
data.sunspots$value <- data.sunspots$value - mean.values

##### Visualisieren #####
par(mfrow = c(1, 1))
plot(data.sunspots$date, data.sunspots$value, type = "l", xlab = "Datum", 
     ylab = "Absolute Sonnenflecken", main = "Sonnenflecken (1770-1869)")

##### Korrelationen betrachten #####
acf(data.sunspots$value, main = "Autokorrelationen der Sunspots Daten")
# Die Wahlen könnten sich hier auf n.lags = 2, 3, 5, 10, 11 belaufen,
# da dort die Korrelationen sehr hoch sind.



##### Modell anpassen #####
# Erhalten der Parameter der Algorithmen
params.dl <- durbin.levinson(data = data.sunspots$value, n.lags = 11)

##### MSE für i=1, ..., 11 berechnen
mse <- c()
for(n in 1:11) {
   dl.pred <- predict.dl(data = data.sunspots$value, params = params.dl$d, n.lags=n, pl = F)
   mse[n] <- dl.pred$mse
}

# quadratische Vorhersagefehler visualisieren
plot(params.dl$v, type = "l", xlab = "n", ylab = TeX("$v_{n}$"), main = "")
points(params.dl$v)

# mse visualisieren
plot(mse, type = "l", xlab ="n", ylab = "MSE")
points(mse)

##### Vorhersagen an den Testdaten berechnen #####
# Wahl von n=2 ist gerechtfertigt
n.lags <- 2
params <- params.dl$d
pred <- predict.dl(data.sunspots$value, params = params, n.lags = n.lags)

# Visualisieren
plot(data.sunspots$date,data.sunspots$value, type = "l", 
     main = "Lineare Vorhersagen mit zwei vorangegangenen Werten", xlab = "Jahr", ylab = "Anzahl Sonnenflecken")
points(data.sunspots$date, pred$pred, type = "l", add = TRUE, col = "red")
legend(1840, 170, legend = c("Wahre Werte", "Vorhersagen"), cex = 0.7, col = c("black", "red"),
       lty=1:2)

# Residuen betrachten
plot(data.sunspots$date,pred$res, main = paste("Residuen, mse = ", pred$mse), xlab = "Jahr", ylab = "Residuum")


##### Vorhersagen für unbekannte Daten #####
data.test <- subset(data.all, date %in% 1910:2020)

# Gleiche Vorprozedur wie bei den Trainingsdaten.
data.test$value <- data.test$value - mean.values

# Vorhersagen treffen
dl.pred.test <- predict.dl(data.test$value, params.dl$d, n.lags = n.lags, pl = F)

plot(data.test$date, data.test$value, type = "l", xlab = "Jahr", ylab = "Anzahl Sonnenflecken", main = "Vorhersagen bei unbekannten Daten")
points(data.test$date, dl.pred.test$pred, type = "l", add = T, col = "red")
legend(1995, 150, legend = c("Wahre Werte", "Vorhersagen"), cex = 0.7, col = c("black", "red"),lty=1:2)

# Residuen betrachten
plot(data.test$date,dl.pred.test$res, main = paste("Residuen, mse = ", dl.pred.test$mse), xlab = "Jahr", ylab = "Residuum")


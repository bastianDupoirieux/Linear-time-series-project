#https://www.insee.fr/fr/statistiques/serie/010537300#Graphique
require(zoo)
require(tseries)
require(fUnitRoots)
library(ggplot2)

data <- "D:/Users/thomas/Documents/GitHub/Series-temportelles/data_010537241.csv"
data_pates <- read.csv(data, sep = ";") # on lit les données, en les différenciant selon le caractèr ;
xm.source <- zoo(data_pates[2]) # la deuxième colonne contient les valeurs
T <- length(xm.source)
seq_int = 1:389
seq_tronquee = 4:389
seq_dates = seq(as.Date("1990/1/1"), as.Date("2022/3/1"), "months")
xm <- xm.source #on commence à partir des premières valeurs numériques, avant il y a des termes générique d'infos
myTimeSeries <- zoo(xm, order.by=seq_int)
myTimeSeries <- myTimeSeries[4:389]#on ne garde que les valeurs numériques

data_pates_diff <- diff(myTimeSeries, 1)
# On s'intéresse d'abord à la série non différenciée
# Test de présence d'une tendance
lm(myTimeSeries ~ seq_tronquee)
# Résultat très significatif : il y a une tendance avec constante

#Fonction servant à effectuer le test du portmanteau sur les résidus
# Mettre ici arima sur série
Box.test(arima_non_diff$residuals, lag=6, type="Ljung-Box", fitdf=5)
# Fonction permettant d'effectuer un test Portmanteau
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))})
  return(t(pvals))
}
# Ne fonctionne pas
Qtests(adf@test$lm$residuals24,length(adf@test$lm$coefficients))
# Conversion en float
myTimeSeriesFloat <- as.numeric(myTimeSeries)
# Test du nbre de lags nécessaires dans la régression de test unitaire pour supprimer l'autocorr des résidus
adfTest_valid <- function(series,kmax,type){ #tests ADF jusqu’`a des r´esidus non autocorr´el´es
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(adf@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")}
    else cat("nope \n")
    k <- k + 1
  }
  return(adf)
}
adf <- adfTest_valid(myTimeSeriesFloat,24,"ct")
# On effectue alors un test ADF avec [adfTest_valid result] lags, de type "ct" car il y a
# Présence d'une tendance dans les données
adf_summary <- adfTest(myTimeSeriesFloat, lag=21, type="ct")
adf_summary
# Série non différenciée : test ADF de p-value Y -> on [...]

#Série différenciée :
seq_test = 1:370
summary(lm(data_pates_diff ~ seq_tronquee[-1]))
# Conclusion : ni tendance ni constante (p-value élevée)
adf <- adfTest_valid(data_pates_diff,24, type="nc")
adf_summary <- adfTest(data_pates_diff, lag=21, type="nc")
adf_summary
# Avec X lags considérés, le test ADF donne une p-value de Y -> on rejette
# La non-stationnarité à tous les niveaux.

# Détermination des ordres
par(mfrow=c(1,2))
acf(data_pates_diff, 20, na.action = na.pass)
pacf(data_pates_diff, 20, na.action = na.pass)
#
# Fitting de différents modèles
arima412 <- arima(myTimeSeries,c(4,1,2)) #enregistre les r´esultats de l’estimation
Box.test(arima412$residuals, lag=6, type="Ljung-Box", fitdf=5)
# Tests portmanteau à différents lags
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
Qtests(arima412$residuals, 24, 5)
# Test rejeté à aucun niveau significatif pour 24 lags
# Evaluation des différents modèles
p_max <- 4
q_max <- 2
mat <- matrix(NA,nrow=p_max+1,ncol=q_max+1) #matrice vide `a remplir
rownames(mat) <- paste0("p=",0:p_max) #renomme les lignes
colnames(mat) <- paste0("q=",0:q_max) #renomme les colonnes
AICs <- mat #matrice des AIC non remplie
BICs <- mat #matrice des BIC non remplie
pqs <- expand.grid(0:p_max,0:q_max) #toutes les combinaisons possibles de p et q
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #r´ecup`ere p
  q <- pqs[row,2] #r´ecup`ere q
  estim <- try(arima(data_boissons_st,c(p,1,q),include.mean = F)) #tente d’estimer l’ARIMA
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #assigne l’AIC
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigne le BIC
}
AICs==min(AICs)
BICs==min(BICs)
# Fitting de l'ARIMA choisi
arima111 <- arima(myTimeSeries,c(1,1,1))
# Qtest sur cet ARIMA
Qtests(arima111$residuals, 24, 1)
# Conclusion

# Prédictions
arima111
get(arima111)

predictions <- predict(arima111,2)$pred
TimeSeriesPred <- myTimeSeries
TimeSeriesPred[387:388] <- c(predictions[1], predictions[2])
TimeSeriesPred
plot(TimeSeriesPred[300:400], ylim=80, xaxt="n")

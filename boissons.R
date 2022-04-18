#https://www.insee.fr/fr/statistiques/serie/010537300#Graphique
require(zoo)
require(tseries)
require(fUnitRoots) 
data_boissons <- read.csv("D:/Users/thomas/Downloads/data_boissons.csv")
dates_seq = seq(as.Date("1990/01"), by = "month", length.out = 386)
seq_int <- 1:386
data_boissons$dates = seq_int
data_boissons <- subset(data_boissons, select=-c(index, AnnÃ.e, Mois, X))
data_boissons_st <- zoo(data_boissons$Valeur,order.by=seq_int)
data_boissons_diff <- diff(data_boissons_st, 1)
plot(cbind(data_boissons_st,data_boissons_diff))
data_boissons_st
# On s'intéresse d'abord à la série non différenciée
# Test de présence d'une tendance
summary(lm(data_boissons_st ~ dates_seq))
# Résultat très significatif : il y a une tendance avec constante
adf <- adfTest(data_boissons_st, lag=19, type="ct")
Qtests <- function(series, k, fitdf=0) {
pvals <- apply(matrix(1:k), 1, FUN=function(l) {
pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
return(c("lag"=l,"pval"=pval))})
return(t(pvals))
}
Qtests(adf@test$lm$residuals24,length(adf@test$lm$coefficients))
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
adf <- adfTest_valid(data_boissons_st,24,"ct")
adf_summary <- adfTest(data_boissons_st, lag=8, type="ct")
adf_summary
#Série différenciée :
summary(lm(data_boissons_diff ~ dates_seq[-1]))
# Conclusion : ni tendance ni constante (p-value élevée)
adf <- adfTest_valid(data_boissons_diff,24, type="nc")
adf_summary <- adfTest(data_boissons_diff, lag=6, type="nc")
adf_summary
# Avec 17 lags considérés, le test ADF augmenté donne une série différenciée stationnaire.

# Détermination des ordres
par(mfrow=c(1,2))
acf(data_boissons_diff, 20, na.action = na.pass)
pacf(data_boissons_diff, 20, na.action = na.pass)
arima(data_boissons_st,c(7,1,1))
# Fitting de différents modèles
arima711 <- arima(data_boissons_st,c(7,1,1)) #enregistre les r´esultats de l’estimation
Box.test(arima711$residuals, lag=6, type="Ljung-Box", fitdf=5)
# Tests portmanteau à différents lags
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
Qtests(arima711$residuals, 24, 5)
# Test rejeté à aucun niveau significatif pour 24 lags
# Evaluation des différents modèles
mat <- matrix(NA,nrow=7+1,ncol=1+1) #matrice vide `a remplir
rownames(mat) <- paste0("p=",0:7) #renomme les lignes
colnames(mat) <- paste0("q=",0:1) #renomme les colonnes
AICs <- mat #matrice des AIC non remplie
BICs <- mat #matrice des BIC non remplie
pqs <- expand.grid(0:7,0:1) #toutes les combinaisons possibles de p et q
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #r´ecup`ere p
  q <- pqs[row,2] #r´ecup`ere q
  estim <- try(arima(data_boissons_st,c(p,1,q),include.mean = F)) #tente d’estimer l’ARIMA
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #assigne l’AIC
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigne le BIC
}
AICs==min(AICs)
BICs==min(BICs)
arima111 <- arima(data_boissons_st,c(1,1,1))
Qtests(arima111$residuals, 24, 1)



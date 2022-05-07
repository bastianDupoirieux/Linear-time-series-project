require(zoo)
require(tseries)
require(fUnitRoots)
require(forecast)


data <- "C:/Users/bastd/Documents/ENSAE/Series_temporelles/Linear-time-series-project/valeurs_mensuelles.csv" # On charge les données
data_avions <- read.csv(data, sep = ";") # on lit les données, en les différenciant selon le ca?actèr ;
xm.source <- zoo(data_avions[1]) # la colonne contenant les valeurs est a deuxième colonne
seq_int = 1:360
date = 5:250

xm <- xm.source #on commence à partir des premières valeurs numériques, avant il y a des termes générique d'infos
myTimeSeries <- zoo(xm, order.by=rev(seq_int))
myTimeSeries <- myTimeSeries[1:360]#on ne garde que les valeurs numériques
myTimeSeries
##PARTIE 1: LES DONNEES

#Q.1: QUE REPRESENTE LA SERIE CHOISIE?
#TODO: description de la serie choisie


#Q.2 TRANSOFRMER LA SERIE POUR ?A RENDRE STATIONNAIRE

plot(myTimeSeries, ylim = c(min(as.numeric(myTimeSeries)) - 1, max(as.numeric(myTimeSeries))  +1))
#TODO: commenter le graphique de la s???rie

#Si on observe une tendance lin???aire, on fait une regression lin???aire pour la mettre en ???vidence
myTimeSeriesFloat <- as.numeric(myTimeSeries)
summary(lm(myTimeSeriesFloat ~ seq_int))

#On doit vérifier la non-autocorrélation des résidus de la régression linéaire, sinon le test ADF n'est pas valide
adf <- adfTest(myTimeSeriesFloat, lag=0, type = "ct")
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))})
  return(t(pvals))
}
Qtests(adf@test$lm$residuals,24,length(adf@test$lm$coefficients))
# Ce premier test ADF sans lag, mais pas valide (il faut 7 lags au test ADF)
# rejette le test de non-autocorrélation à tous les niveaux, pour tous les ordres.
# On inclut donc plus d'ordre

# Fonction de calcul de test ADF avec k lags
adfTest_valid <- function(series,kmax,type){ #tests ADF jusqu'`a des r´esidus non autocorr´el´es
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <-Qtests(adf@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")}
    else cat("nope \n")
    k <- k + 1
  }
  return(adf)
}

adf <- adfTest_valid(myTimeSeriesFloat,24,"ct")

# Avec 24 ordres à chaque fois, il faut considérer 12 lags pour obtenir un test valide.
# Affichons alors les conclusions de ce test valide :
adfTest(myTimeSeriesFloat, lag=7, type="ct")
# p-value de 0.1595 pour la test précédent -> on ne peut pas rej?ter à 5 % la non-stationnarité
#On étudie donc la série différenciée
myTimeSeriesDiff <- diff(myTimeSeriesFloat, 1)
summary(lm(formula = myTimeSeriesDiff ~ seq_int[-1]))
# p-valeurs très significatives, la série différenciée semble ne présenter aucune tend?nce
# on vérifie cela en tenant compte de la possible autocorrélation des résidus dans
# la régression ci-dessus : 
adf <- adfTest_valid(myTimeSeriesDiff,24, type="nc")
# Il faut 12 lags pour supprimer l'autocorrélation
adf <- adfTest(myTimeSeriesDiff, lag?6, type="nc")
adf
# Le test ADF rejette la non-stationnarité à tous les niveaux, la série différenciée
# est donc stationnaire.

#Q.3 REPRESENTER GRAPHIQUEMENT LES SERIES AVANT ET APRES TRANSFORMATION
#Représentation graphique de la série avant transformat?on
plot(myTimeSeries, ylim = as.numeric(max(myTimeSeries)) + 1)#on trace la série temporelle
axis(side=1, at=seq(1,385, 12)) # on rajoute les mois par pas de 12 en axe des abscisses

plot(myTimeSeriesDiff, type = 'l')

##PARTIE 2: MODELES ARMA

#Q.4: CHOIS?R UN MODELE ARMA EN JUSTIFIANT 

# On commence par determiner les ordres
par(mfrow=c(1,2))
acf(myTimeSeriesDiff, 25, na.action = na.pass)
pacf(myTimeSeriesDiff, 25, na.action = na.pass)

# On teste alors p_max = 11 et q_max = 14
arimafit <- function(estim){
  pvals <- Qtests(estim$residuals,24,length(estim$coef)-1)
  pvals <- matrix(apply(matrix(1:24,nrow=6),2,function(c) round(pvals[c,],3)),nrow=6)
  colnames(pvals) <- rep(c("lag", "pval"),4)
  cat("tests de nullit´e des coefficients :\n")
  print(estim)
  ?at("\n tests d'absence d'autocorr´elation des r´esidus : \n")
  print(pvals)
}

p_max <- 11
q_max <- 14

# Test de tous les modèles p <= 11 et q <= 14.

#for (p in 1:p_max){
  #for (q in 1:q_max){
    #estim <- arima(as.numeric(myTimeSeries),c(p,1,q),inclu?e.mean = F, optim.method="Nelder-Mead", xreg=1:length(myTimeSeries))
    #arimafit(estim)
  #}
#}

# Il y en a trop, on sélectionne donc
# Directement en minimisant AIC et BIC.

mat <- matrix(NA,nrow=p_max+1,ncol=q_max+1) #matrice vide `a remplir
rownames(mat) <- paste0("p=",0:p_max) #renomme les lignes
colnames(mat) <- paste0("q=",0:q_max) #renomme les colonnes
AICs <- mat #matrice des AIC non remplie
BICs <- mat #matrice des BIC non remplie
pqs <- expand.grid(0:p_max,0:q_max) #toutes les combinaisons possi?les de p et q
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #r´ecup`ere p
  q <- pqs[row,2] #r´ecup`ere q
  estim <- try(arima(as.numeric(myTimeSeries),c(p,1,q),include.mean = F, optim.method="Nelder-Mead", xreg=1:length(myTimeSeries))) #tente d'estimer l'ARIMA
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #assigne l'AIC
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigne le BIC
}
AICs==min(AICs)
BICs==min(BICs)


# On teste le modèle ?aximal trouvé
arima_max <- arima(as.numeric(myTimeSeries),c(11, 1, 14))
Qtests(arima_max$residuals, 48, 25)
arima_max
# On prend des lags sur 4 ans, et on obtient les tests portmanteaux fonctionnent à partir du lag 29
#Q.5: EXPRIMER LE MODELE ARIMA(p,d,q) ?OUR LA SERIE CHOISIE
#Donc on a un ARIMA(2,1,2) et un ARIMA(0, 1, 1) qu'on teste ici
arima011 <- arima(as.numeric(myTimeSeries),c(0,1,1))
arima212 <- arima(as.numeric(myTimeSeries),c(2,1,2))
# Qtest sur ces ARIMA
arima011 # Bien ajusté
arima212 # Bien ajus?é
Qtests(arima011$residuals, 24, 1) # Rejeté
Qtests(arima212$residuals, 24, 4) # Pas complètement rejeté

# Conclusion: l'ARIMA(2, 1, 2) a presque toutes ses autocorrélations qui passent le test
# Donc on retient ce modèle

##PARTIE 3: PREVISIONS

#QUESTIO? 6: ECRIRE L'EQUATION VERIFIEE PAR LA REGION DE CONFIANCE

#QUESTION 7: PRECISER LES HYPOTHESES UTILISEES POUR OBTENIR CETTE REGION

#QUESTION 8: REPRESENTER GRAPHIQUEMENT CETTE REGION POUR \alpha = 95% ET COMMENTER
arima212

forecastedValues <- forecast(arima212, h = 2, level = 0.95, model = 'Arima')# on utilise un ARIMA pour predire les deux prochaines valeurs au seuil de 0.95

timeSeriesPredictions <- append(as.character(myTimeSeries), c(forecastedValues$Forecast))

#Finalement, on represente graphiquement la serie temporelle et ses nouvelles predictions, en grisant les intervalles de confiance
plot(forecastedValues, shaded = TRUE)

#on tronque sur les 50 dernières valeurs, et on compare la prédiction aux valeurs réelles (COVID)
extraData <- "C:/Users/bastd/Documents/ENSAE/Series_temporelles/Linear-time-series-project/valeurs_mensuelles_mars.csv"
extraDf <- read.csv(extraData, sep = ";")
temp <- zoo(extraDf[2])

plot(forecastedValues, include = 50, shaded = TRUE) 

plot(forecastedValues, include = 10, shaded = TRUE, ylim = c(75,150), type = 'l')
points(x = c(361, 362), y = rev(c(temp[5], temp[4])), pch = 23, col = 'red')
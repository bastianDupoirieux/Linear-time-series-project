require(zoo)
require(tseries)
require(fUnitRoots)


data <- "D:/Users/thomas/Documents/GitHub/Series-temportelles/data_pates.csv" # On charge les données
data_pates <- read.csv(data, sep = ";") # on lit les données, en les différenciant selon le caractèr ;?xm.source <- zoo(data_pates[2]) # la colonne contenant les valeurs est a deuxième colonne
seq_int = 1:389
date = 4:389

xm <- xm.source #on commence à partir des premières valeurs numériques, avant il y a des termes générique d'infos
myTimeSeries <- zoo(xm? order.by=seq_int)
myTimeSeries <- myTimeSeries[4:389]#on ne garde que les valeurs numériques
myTimeSeries
##PARTIE 1: LES DONNEES

#Q.1: QUE REPRESENTE LA SERIE CHOISIE?
#TODO: description de la serie choisie


#Q.2 TRANSOFRMER LA SERIE POUR LA RENDRE STA?IONNAIRE

plot(myTimeSeries, ylim = as.numeric(max(myTimeSeries)) + 1)
#TODO: commenter le graphique de la s???rie

#Si on observe une tendance lin???aire, on fait une regression lin???aire pour la mettre en ???vidence
summary(lm(myTimeSeries ~ date))

#On doit vérifier la non-autocorrélation des résidus de la régression linéaire, sinon le test ADF n'est pas valide
myTimeSeriesFloat <- as.numeric(myTimeSeries)
adf <- adfTest(myTimeSeriesFloat, lag=0, type="ct")
Q?ests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))})
  return(t(pvals))
}
Qtests(adf@?est$lm$residuals,24,length(adf@test$lm$coefficients))
# Ce premier test ADF sans lag
# rejette le test de non-autocorrélation à tous les niveaux, pour tous les ordres.
# On inclut donc plus d'ordre

# Fonction de calcul de test ADF avec k lags
adfTest_vali? <- function(series,kmax,type){ #tests ADF jusqu'`a des r´esidus non autocorr´el´es
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(?df@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")}
    else cat("nope \n")
    k <- k + 1
  }
  return(adf)
}

adf <- adfTest_valid(myTimeSeriesFloat,24,"ct")

# ?vec 24 ordres à chaque fois, il faut considérer 12 lags pour obtenir un test valide.
# Affichons alors les conclusions de ce test valide :
adfTest(myTimeSeriesFloat, lag=12, type="ct")

#On commence par ???tudier la s???rie non-diff???renci???e


#on commence par tester la présence d'une tendance avec un adf-test
adfTrend <- adfTest(as.numeric(myTimeSeries), lag = 25, type = 'ct') #on teste la présence d'une tendance, en transformant les valeurs de la séries en valuer numériques
#La série prése?te une tendance. On va donc différencier la série, puis revérifier si elle est bien stationnaire et sans tendance


#Il faut donc enlever la tendance observée dans la série. Pour ca on différencie la série une première fois
myDifferentiatedTimeSeries <- di?f(as.numeric(myTimeSeries), 1)

adfTrendDifferentiated <- adfTest(as.numeric(myDifferentiatedTimeSeries), lag = 25, type = 'ct')

#Pour transformer la série en série stationnaire sans tendance, il a suffit d'une seule différentiation


#Q.3 REPRESENTER GRA?HIQUEMENT LES SERIES AVANT ET APRES TRANSFORMATION
#Représentation graphique de la série avant transformation
plot(myTimeSeries, ylim = as.numeric(max(myTimeSeries)) + 1)#on trace la série temporelle
axis(side=1, at=seq(1,385, 12)) # on rajoute les mois pa? pas de 12 en axe des abscisses

plot(myDifferentiatedTimeSeries, type = 'l')

##PARTIE 2: MODELES ARMA

#Q.4: CHOISIR UN MODELE ARMA EN JUSTIFIANT 

# On commence par determiner les ordres
par(mfrow=c(1,2))
acf(myDifferentiatedTimeSeries, 20, na.action = ?a.pass)
pacf(myDifferentiatedTimeSeries, 20, na.action = na.pass)

# Fitting de différents modèles
arima412 <- arima(as.numeric(myTimeSeries),c(4,1,2)) #enregistre les r´esultats de l'estimation
Box.test(arima412$residuals, lag=6, type="Ljung-Box", fitdf=5?
# Tests portmanteau à différents lags
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=?val))
  })
  return(t(pvals))
}
Qtests(arima412$residuals, 24, 5)
# Test rejeté à aucun niveau significatif pour 24 lags
# Evaluation des différents modèles

p_max <- 4
q_max <- 2
mat <- matrix(NA,nrow=p_max+1,ncol=q_max+1) #matrice vide `a remplir
rowname?(mat) <- paste0("p=",0:p_max) #renomme les lignes
colnames(mat) <- paste0("q=",0:q_max) #renomme les colonnes
AICs <- mat #matrice des AIC non remplie
BICs <- mat #matrice des BIC non remplie
pqs <- expand.grid(0:p_max,0:q_max) #toutes les combinaisons pos?ibles de p et q
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #r´ecup`ere p
  q <- pqs[row,2] #r´ecup`ere q
  estim <- try(arima(as.numeric(myTimeSeries),c(p,1,q),include.mean = F)) #tente d'estimer l'ARIMA
  AICs[p+1,q+1] <- if (?lass(estim)=="try-error") NA else estim$aic #assigne l'AIC
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigne le BIC
}
AICs==min(AICs)
BICs==min(BICs)
#Donc on a un ARMA(1,1)

#Q.5: EXPRIMER LE MODELE ARIMA(p,d,q) POUR LA SERIE C?OISIE
arima111 <- arima(as.numeric(myTimeSeries),c(1,1,1))
# Qtest sur cet ARIMA
Qtests(arima111$residuals, 24, 1)
# Conclusion: on a un ARIMA (1, d, 1)

##PARTIE 3: PREVISIONS

#QUESTION 6: ECRIRE L'EQUATION VERIFIEE PAR LA REGION DE CONFIANCE
#blabla

#Q?ESTION 7: PRECISER LES HYPOTHESES UTILISEES POUR OBTENIR CETTE REGION

#QUESTION 8: REPRESENTER GRAPHIQUEMENT CETTE REGION POUR \alpha = 95% ET COMMENTER
arima111


predictions <- predict(arima111, level = 0.95, 2) #on utilise la fonction predict, au nivea? 0.95 pour pr???dire les 2 prochaines valeurs
timeSeriesPredictions <- append(myTimeSeries, c(predictions$pred))

#Finalement, on represente graphiquement la serie temporelle et ses nouvelles predictions
plot(timeSeriesPredictions, ylim = as.numeric(max(timeSeriesPredicti?ns)) + 1)
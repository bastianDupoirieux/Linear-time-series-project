require(zoo)
require(tseries)
require(fUnitRoots)


data <- "data_pates.csv" # On charge les données
data_pates <- read.csv(data, sep = ";") # on lit les données, en les différenciant selon le caractèr ;
xm.source <- zoo(data_pates[2]) # la colonne contenant les valeurs est a deuxième colonne
T <- length(xm.source)
seq_int = 1:389
xm <- xm.source #on commence à partir des premières valeurs numériques, avant il y a des termes générique d'infos
myTimeSeries <- zoo(xm, order.by=seq_int)
myTimeSeries <- myTimeSeries[5:389]#on ne garde que les valeurs numériques

##PARTIE 1: LES DONNEES

#Q.1: QUE REPRESENTE LA SERIE CHOISIE?
#La série est une série représentant les ventes de pâtes alimentaires de 1990 à fevrier 2021.
# Elle concerne le secteur de l'agroalimentaire
#Elle est réalisée à partir d'enquêtes mensuelles de l'INSEE dans le secteur de l'agroalimentaire
# Elle a été traitée initialement pour y enlever les tendances saisonnières et les effets de calendrier (prise en compte des jours ouvrés)

#Q.2 TRANSOFRMER LA SERIE POUR LA RENDRE STATIONNAIRE

#on commence par tester la présence d'une tendance
adfTrend <- adfTest(as.numeric(myTimeSeries), lag = 25, type = 'ct') #on teste la présence d'une tendance, en transformant les valeurs de la séries en valuer numériques
#La série présente une tendance. On va donc différencier la série, puis revérifier si elle est bien stationnaire et sans tendance

#Il faut donc enlever la tendance observée dans la série. Pour ca on différencie la série une première fois
myDifferentiatedTimeSeries <- diff(as.numeric(myTimeSeries), 1)
#On reteste la présence d'une tendance
adfTrend <- adfTest(as.numeric(myDifferentiatedTimeSeries), lag = 25, type = 'ct')
#La p-valeur vaut maintenant moins de 0.01, donc il n'y a plus de tendance
#On teste maintenant la stationarité de la série différenciée
adfStationary <- adf.test(as.numeric(myDifferentiatedTimeSeries))
#La p-value < 0.01, donc la série est stationaire

#Pour transformer la série en série stationnaire sans tendance, il a suffit d'une seule différentiation

#On regarde les fonctions d'autocorrelation
acf(myDifferentiatedTimeSeries, lag.max = 50)
ppacf(myDifferentiatedTimeSeries, lag.max = 50)
#Il semblerait y avoir une saisonalité


#Q.3 REPRESENTER GRAPHIQUEMENT LES SERIES AVANT ET APRES TRANSFORMATION
#Représentation graphique de la série avant transformation
y_lim <- as.numeric(max(myTimeSeries)) + 1 #on définit un terme qui nous permettra de tracer le graphique
plot(myTimeSeries, ylim = y_lim, xaxt="n")#on trace la série temporelle
axis(side=1, at=seq(1,385, 12)) # on rajoute les mois par pas de 12 en axe des abscisses
#On n'a pas besoin de représenter la série stationnaire

#On veut tracer les ACF et PACF de la série
#On indexe maintenant la série sur des entiers, pour pouvoir tracer les ACF
dates_seq = 1:389
data_pates$observation = dates_seq #on séquence les obsevations pour pouvoir calculer les ACF



##PARTIE 2: MODELES ARMA

#Q.4: CHOISIR UN MODELE ARMA EN JUSTIFIANT 


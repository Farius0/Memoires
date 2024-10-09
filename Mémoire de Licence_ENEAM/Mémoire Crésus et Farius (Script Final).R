###### Analyse des donn?es du m?moire 

Modele1 <- read.table("clipboard", header=TRUE, dec=",", sep="\t", row.names = 1)#### Importation de la base (Copier la base du mod?le 1 Final dans excel puis ex?cuter la commande)

attach(Modele1)#### acc?s au variable de la base

names(Modele1)#### nom des variable de la base

###### Biblioth?que

library('urca')#### Compilation du package

library('tseries')#### Compilation du package

library('tsDyn')#### Compilation du package

library('quantmod')#### Compilation du package

library('lmtest')#### Compilation du package

library('aTSA')#### Compilation du package

library('qcc')#### Compilation du package

library('vars')#### Compilation du package

library('car')#### Compilation du package

library('orcutt')#### Compilation du package

library('mctest')#### Compilation du package

library('ppcor')#### Compilation du package

############################################Stationnarit? des variables(TEST ADF)

Modele1.ts <- ts(Modele1,start = 1980, end = 2017, frequency = 1)#### transformation de la base en base de s?rie temporelle

plot.ts(Modele1.ts)#### courbe des variables du mod?le en fonction du temps

#### 1-Test d'ADF sur l'indice de diversification(SPE)

SPEts <- ts(SPE,start = 1980, end = 2017, frequency = 1)#### transformation SPE variable en s?rie temporelle SPEts

dSPEts <- diff(SPEts)#### Cr?ation de la variable SPE diff?rienci?

ddSPEts <- diff(dSPEts)#### Cr?ation de la variable SPE deux fois diff?renci?

pacf(SPEts)#### Corr?logramme partielle de la variable SPEts

pacf(dSPEts)#### Corr?logramme partielle de la variable dSPEts utilis? pour le choix du lag ou retard optimal de la variable SPEts

pacf(ddSPEts)#### Corr?logramme partielle de la variable ddSPEts utilis? pour le choix du lag ou retard optimal de la variable dSPEts

plot.ts(SPEts ,col="blue",main="SPEts",xlab="ann?e",ylab="SPEts")#### Graphe de la variable SPEts

plot.ts(dSPEts ,col="blue",main="dSPEts",xlab="ann?e",ylab="dSPEts")#### Graphe de la variable dSPEts

plot.ts(ddSPEts ,col="blue",main="ddSPEts",xlab="ann?e",ylab="ddSPEts")#### Graphe de la variable ddSPEts

model3SPEts <- ur.df(SPEts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable SPEts avec un retard optimal de 0

summary(model3SPEts)#### Affichage r?sultats du model3SPEts

model2SPEts <- ur.df(SPEts, type = "drift", lags = 1)#### Test de dickey fuller sans tendance avec constante sur la variable SPEts avec un retard optimal de 0

summary(model2SPEts)#### Affichage r?sultats du model2SPEts

model1SPEts <- ur.df(SPEts, type = "none", lags = 0)#### Test de dickey fuller sans tendance sans constante sur la variable SPEts avec un retard optimal de 0

summary(model1SPEts)#### Affichage r?sultats du model1SPEts

model3dSPEts <- ur.df(dSPEts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable dSPEts avec un retard optimal de 1

summary(model3dSPEts)#### Affichage r?sultats du model3dSPEts

model2dSPEts <- ur.df(dSPEts, type = "drift", lags = 1)#### Test de dickey fuller sans tendance avec constante sur la variable dSPEts avec un retard optimal de 1

summary(model2dSPEts)#### Affichage r?sultats du model2dSPEts

model1dSPEts <- ur.df(dSPEts, type = "none", lags = 1)#### Test de dickey fuller sans tendance sans constante sur la variable dSPEts avec un retard optimal de 1

summary(model1dSPEts)#### Affichage r?sultats du model1dSPEts

#### 2-Test d'ADF sur l'investissement agricole(Inva)

Inva.....ts <- ts(Inva.....,start = 1980, end = 2017, frequency = 1)#### transformation variable Inva..... en s?rie temporelle Inva.....ts

dInva.....ts <- diff(Inva.....ts)#### Cr?ation de la variable Inva.....ts diff?rienci?

ddInva.....ts <- diff(dInva.....ts)#### Cr?ation de la variable Inva.....ts deux fois diff?renci?

pacf(Inva.....ts)#### Corr?logramme partielle de la variable Inva.....ts

pacf(dInva.....ts)#### Corr?logramme partielle de la variable dInva.....ts utilis? pour le choix du lag optimal de la variable Inva.....ts

pacf(ddInva.....ts)#### Corr?logramme partielle de la variable ddInva.....ts utilis? pour le choix du lag optimal de la variable dInva.....ts

plot.ts(Inva.....ts ,col="blue",main="Inva.....ts",xlab="ann?e",ylab="Inva.....ts")#### Graphe de la variable Inva.....ts

plot.ts(dInva.....ts ,col="blue",main="dInva.....ts",xlab="ann?e",ylab="dInva.....ts")#### Graphe de la variable dInva.....ts

plot.ts(ddInva.....ts ,col="blue",main="ddInva.....ts",xlab="ann?e",ylab="ddInva.....ts")#### Graphe de la variable ddInva.....ts

model3Inva.....ts<- ur.df(Inva.....ts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable Inva.....ts avec un retard optimal de 1

summary(model3Inva.....ts)#### Affichage r?sultats du model3Inva.....ts

model2Inva.....ts<- ur.df(Inva.....ts, type = "drift", lags = 1)#### Test de dickey fuller sans tendance avec constante sur la variable Inva.....ts avec un retard optimal de 1

summary(model2Inva.....ts)#### Affichage r?sultats du model2Inva.....ts

model3dInva.....ts<- ur.df(dInva.....ts, type = "trend", lags = 3)#### Test de dickey fuller avec tendance avec constante sur la variable dInva.....ts avec un retard optimal de 3

summary(model3dInva.....ts)#### Affichage r?sultats du model3dInva.....ts

model2dInva.....ts<- ur.df(dInva.....ts, type = "drift", lags = 3)#### Test de dickey fuller sans tendance avec constante sur la variable dInva.....ts avec un retard optimal de 3

summary(model2dInva.....ts)#### Affichage r?sultats du model2dInva.....ts

model1dInva.....ts<- ur.df(dInva.....ts, type = "none", lags = 3)#### Test de dickey fuller sans tendance sans constante sur la variable dInva.....ts avec un retard optimal de 3

summary(model1dInva.....ts)#### Affichage r?sultats du model1dInva.....ts

#### 3-Test d'ADF sur le PIB agricole(PIBa) en pourcentage du PIB

PIBa.....ts <- ts(PIBa.....,start = 1980, end = 2017, frequency = 1)#### transformation variable PIBa..... en s?rie temporelle PIBa.....ts

dPIBa.....ts <- diff(PIBa.....ts)#### Cr?ation de la variable PIBa.....ts diff?rienci?

ddPIBa.....ts <- diff(dPIBa.....ts)#### Cr?ation de la variable PIBa.....ts deux fois diff?renci?

pacf(PIBa.....ts)#### Corr?logramme partielle de la variable PIBa.....ts

pacf(dPIBa.....ts)#### Corr?logramme partielle de la variable dPIBa.....ts utilis? pour le choix du lag optimal de la variable PIBa.....ts

pacf(ddPIBa.....ts)#### Corr?logramme partielle de la variable ddPIBa.....ts utilis? pour le choix du lag optimal de la variable dPIBa.....ts

plot.ts(PIBa.....ts ,col="blue",main="PIBa.....ts",xlab="ann?e",ylab="PIBa.....ts")#### Graphe de la variable PIBa.....ts

plot.ts(dPIBa.....ts ,col="blue",main="dPIBa.....ts",xlab="ann?e",ylab="dPIBa.....ts")#### Graphe de la variable dPIBa.....ts

plot.ts(ddPIBa.....ts ,col="blue",main="ddPIBa.....ts",xlab="ann?e",ylab="ddPIBa.....ts")#### Graphe de la variable ddPIBa.....ts

model3PIBa.....ts <- ur.df(PIBa.....ts, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable PIBa.....ts avec un retard optimal de 0

summary(model3PIBa.....ts)#### Affichage r?sultats du model3PIBa.....ts

model3dPIBa.....ts <- ur.df(dPIBa.....ts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable dPIBa.....ts avec un retard optimal de 1

summary(model3dPIBa.....ts)#### Affichage r?sultats du model3dPIBa.....ts

model2dPIBa.....ts <- ur.df(dPIBa.....ts, type = "drift", lags = 1)#### Test de dickey fuller sans tendance avec constante sur la variable dPIBa.....ts avec un retard optimal de 1

summary(model2dPIBa.....ts)#### Affichage r?sultats du model2dPIBa.....ts

model1dPIBa.....ts <- ur.df(dPIBa.....ts, type = "none", lags = 1)#### Test de dickey fuller sans tendance sans constante sur la variable dPIBa.....ts avec un retard optimal de 1

summary(model1dPIBa.....ts)#### Affichage r?sultats du model1dPIBa.....ts

#### 4-Test d'ADF sur le taux d'ouverture

Taux.d.ouverturets <- ts(Taux.d.ouverture,start = 1980, end = 2017, frequency = 1)#### transformation variable Taux.d.ouverture en s?rie temporelle Taux.d.ouverturets

dTaux.d.ouverturets <- diff(Taux.d.ouverturets)#### Cr?ation de la variable Taux.d.ouverturets diff?rienci?

ddTaux.d.ouverturets <- diff(dTaux.d.ouverturets)####Cr?ation de la variable Taux.d.ouverturets deux fois diff?renci?

pacf(Taux.d.ouverturets)#### Corr?logramme partielle de la variable Taux.d.ouverturets

pacf(dTaux.d.ouverturets)#### Corr?logramme partielle de la variable dTaux.d.ouverturets utilis? pour le choix du lag optimal de la variable Taux.d.ouverturets

pacf(ddTaux.d.ouverturets)#### Corr?logramme partielle de la variable ddTaux.d.ouverturets utilis? pour le choix du lag optimal de la variable dTaux.d.ouverturets

plot.ts(Taux.d.ouverturets,col="blue",main="Taux.d.ouverturets",xlab="ann?e",ylab="Taux.d.ouverturets")#### Graphe de la variable Taux.d.ouverturets

plot.ts(dTaux.d.ouverturets,col="blue",main="dTaux.d.ouverturets",xlab="ann?e",ylab="dTaux.d.ouverturets")#### Graphe de la variable dTaux.d.ouverturets

plot.ts(ddTaux.d.ouverturets,col="blue",main="ddTaux.d.ouverturets",xlab="ann?e",ylab="ddTaux.d.ouverturets")#### Graphe de la variable ddTaux.d.ouverturets

model3Taux.d.ouverturets <- ur.df(Taux.d.ouverturets, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable Taux.d.ouverturets avec un retard optimal de 0

summary(model3Taux.d.ouverturets)#### Affichage r?sultats du model3Taux.d.ouverturets

model2Taux.d.ouverturets <- ur.df(Taux.d.ouverturets, type = "drift", lags = 0)#### Test de dickey fuller sans tendance avec constante sur la variable Taux.d.ouverturets avec un retard optimal de 0

summary(model2Taux.d.ouverturets)#### Affichage r?sultats du model2Taux.d.ouverturets

model1Taux.d.ouverturets <- ur.df(Taux.d.ouverturets, type = "none", lags = 0)#### Test de dickey fuller sans tendance sans constante sur la variable Taux.d.ouverturets avec un retard optimal de 0

summary(model1Taux.d.ouverturets)#### Affichage r?sultats du model1Taux.d.ouverturets

model3dTaux.d.ouverturets<- ur.df(dTaux.d.ouverturets, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable dTaux.d.ouverturets avec un retard optimal de 2

summary(model3dTaux.d.ouverturets)#### Affichage r?sultats du model3dTaux.d.ouverturets

model2dTaux.d.ouverturets<- ur.df(dTaux.d.ouverturets, type = "drift", lags = 2)#### Test de dickey fuller sans tendance avec constante sur la variable dTaux.d.ouverturets avec un retard optimal de 2

summary(model2dTaux.d.ouverturets)#### Affichage r?sultats du model2dTaux.d.ouverturets

model1dTaux.d.ouverturets<- ur.df(dTaux.d.ouverturets, type = "none", lags = 2)#### Test de dickey fuller sans tendance sans constante sur la variable dTaux.d.ouverturets avec un retard optimal de 2

summary(model1dTaux.d.ouverturets)#### Affichage r?sultats du model1dTaux.d.ouverturets

#### 5-Test d'ADF sur l'inflation

inflation..d?flacteur.du.PIB....annuel.ts <- ts(inflation..d.flacteur.du.PIB....annuel.,start = 1980, end = 2017, frequency = 1)#### transformation variable inflation..d?flacteur.du.PIB....annuel. en s?rie temporelle inflation..d?flacteur.du.PIB....annuel.ts

dinflation..d?flacteur.du.PIB....annuel.ts<- diff(inflation..d?flacteur.du.PIB....annuel.ts)#### Cr?ation de la variable inflation..d?flacteur.du.PIB....annuel.ts diff?rienci?

ddinflation..d?flacteur.du.PIB....annuel.ts <- diff(dinflation..d?flacteur.du.PIB....annuel.ts)####Cr?ation de la variable inflation..d?flacteur.du.PIB....annuel.ts deux fois diff?renci?

pacf(inflation..d?flacteur.du.PIB....annuel.ts)#### Corr?logramme partielle de la variable inflation..d?flacteur.du.PIB....annuel.ts

pacf(dinflation..d?flacteur.du.PIB....annuel.ts)#### Corr?logramme partielle de la variable dinflation..d?flacteur.du.PIB....annuel.ts utilis? pour le choix du lag optimal de la variable inflation..d?flacteur.du.PIB....annuel.ts

pacf(ddinflation..d?flacteur.du.PIB....annuel.ts)#### Corr?logramme partielle de la variable ddinflation..d?flacteur.du.PIB....annuel.ts utilis? pour le choix du lag optimal de la variable dinflation..d?flacteur.du.PIB....annuel.ts

plot.ts(inflation..d?flacteur.du.PIB....annuel.ts ,col="blue",main="inflation..d?flacteur.du.PIB....annuel.ts",xlab="ann?e",ylab="inflation..d?flacteur.du.PIB....annuel.ts")#### Graphe de la variable inflation..d?flacteur.du.PIB....annuel.ts

plot.ts(dinflation..d?flacteur.du.PIB....annuel.ts ,col="blue",main="dinflation..d?flacteur.du.PIB....annuel.ts",xlab="ann?e",ylab="dinflation..d?flacteur.du.PIB....annuel.ts")#### Graphe de la variable dinflation..d?flacteur.du.PIB....annuel.ts

plot.ts(ddinflation..d?flacteur.du.PIB....annuel.ts ,col="blue",main="ddinflation..d?flacteur.du.PIB....annuel.ts",xlab="ann?e",ylab="ddinflation..d?flacteur.du.PIB....annuel.ts")#### Graphe de la variable ddinflation..d?flacteur.du.PIB....annuel.ts

model3inflation..d?flacteur.du.PIB....annuel.ts <- ur.df(inflation..d?flacteur.du.PIB....annuel.ts, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable inflation..d?flacteur.du.PIB....annuel.ts avec un retard optimal de 0

summary(model3inflation..d?flacteur.du.PIB....annuel.ts)#### Affichage r?sultats du model3inflation..d?flacteur.du.PIB....annuel.ts

model2inflation..d?flacteur.du.PIB....annuel.ts <- ur.df(inflation..d?flacteur.du.PIB....annuel.ts, type = "drift", lags = 0)#### Test de dickey fuller sans tendance avec constante sur la variable inflation..d?flacteur.du.PIB....annuel.ts avec un retard optimal de 0

summary(model2inflation..d?flacteur.du.PIB....annuel.ts)#### Affichage r?sultats du model2inflation..d?flacteur.du.PIB....annuel.ts

#### 6-Test d'ADF sur le taux de change (Tchan)

Tchants <- ts(Tchan,start = 1980, end = 2017, frequency = 1)#### transformation variable Tchan en s?rie temporelle Tchants

dTchants <- diff(Tchants)#### Cr?ation de la variable Tchants diff?rienci?

ddTchants<- diff(dTchants)#### Cr?ation de la variable Tchants deux fois diff?renci?

pacf(Tchants)#### Corr?logramme partielle de la variable Tchants

pacf(dTchants)#### Corr?logramme partielle de la variable dTchants utilis? pour le choix du lag optimal de la variable Tchants

pacf(ddTchants)#### Corr?logramme partielle de la variable ddTchants utilis? pour le choix du lag optimal de la variable dTchants

plot.ts(Tchants ,col="blue",main="Tchants",xlab="ann?e",ylab="Tchants")#### Graphe de la variable Tchants

plot.ts(dTchants ,col="blue",main="dTchants",xlab="ann?e",ylab="dTchants")#### Graphe de la variable dTchants

plot.ts(ddTchants ,col="blue",main="ddTchants",xlab="ann?e",ylab="ddTchants")#### Graphe de la variable ddTchants

model3Tchants <- ur.df(Tchants, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable Tchants avec un retard optimal de 0

summary(model3Tchants)#### Affichage r?sultats du model3Tchants

model2Tchants <- ur.df(Tchants, type = "drift", lags = 0)#### Test de dickey fuller sans tendance avec constante sur la variable Tchants avec un retard optimal de 0

summary(model2Tchants)#### Affichage r?sultats du model2Tchants

model3dTchants <- ur.df(dTchants, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable dTchants avec un retard optimal de 2

summary(model3dTchants)#### Affichage r?sultats du model3dTchants

model2dTchants <- ur.df(dTchants, type = "drift", lags = 2)#### Test de dickey fuller sans tendance avec constante sur la variable dTchants avec un retard optimal de 2

summary(model2dTchants)#### Affichage r?sultats du model2dTchants

model1dTchants <- ur.df(dTchants, type = "none", lags = 2)#### Test de dickey fuller sans tendance sans constante sur la variable dTchants avec un retard optimal de 2

summary(model1dTchants)#### Affichage r?sultats du model1dTchants

#### 7-Test d'ADF sur la situation politique et l'absence de conflit

Spots <- ts(Spo,start = 1980, end = 2017, frequency = 1)#### transformation variable Spo en s?rie temporelle Spots

dSpots <- diff(Spots)#### Cr?ation de la variable Spots diff?rienci?

ddSpots<- diff(dSpots)#### Cr?ation de la variable Spots deux fois diff?renci?

pacf(Spots)#### Corr?logramme partielle de la variable Spots

pacf(dSpots)#### Corr?logramme partielle de la variable dSpots utilis? pour le choix du lag optimal de la variable Spots

pacf(ddSpots)#### Corr?logramme partielle de la variable ddSpots utilis? pour le choix du lag optimal de la variable dSpots

plot.ts(Spots ,col="blue",main="Spots",xlab="ann?e",ylab="Spots")#### Graphe de la variable Spots

plot.ts(dSpots ,col="blue",main="dSpots",xlab="ann?e",ylab="dSpots")#### Graphe de la variable dSpots

plot.ts(ddSpots ,col="blue",main="ddSpots",xlab="ann?e",ylab="ddSpots")#### Graphe de la variable ddSpots

model3Spots <- ur.df(Spots, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable Spots avec un retard optimal de 0

summary(model3Spots)#### Affichage r?sultats du model3Spots

model2Spots <- ur.df(Spots, type = "drift", lags = 0)#### Test de dickey fuller sans tendance avec constante sur la variable Spots avec un retard optimal de 0

summary(model2Spots)#### Affichage r?sultats du model2Spots

model1Spots <- ur.df(Spots, type = "none", lags = 0)#### Test de dickey fuller sans tendance sans constante sur la variable Spots avec un retard optimal de 0

summary(model1Spots)#### Affichage r?sultats du model1Spots

model3dSpots <- ur.df(dSpots, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable dSpots avec un retard optimal de 2

summary(model3dSpots)#### Affichage r?sultats du model3dSpots

model2dSpots <- ur.df(dSpots, type = "drift", lags = 2)#### Test de dickey fuller sans tendance avec constante sur la variable dSpots avec un retard optimal de 2

summary(model2dSpots)#### Affichage r?sultats du model2dSpots

model1dSpots <- ur.df(dSpots, type = "none", lags = 2)#### Test de dickey fuller sans tendance sans constante sur la variable dSpots avec un retard optimal de 2

summary(model1dSpots)#### Affichage r?sultats du model1dSpots

#### 8-Test d'ADF sur le solde budg?taire

Sbugt.....ts <- ts(Sbugt.....,start = 1980, end = 2017, frequency = 1)#### transformation variable Sbugt..... en s?rie temporelle Sbugt.....ts

dSbugt.....ts<- diff(Sbugt.....ts)#### Cr?ation de la variable Sbugt.....ts diff?rienci?

ddSbugt.....ts<- diff(dSbugt.....ts)#### Cr?ation de la variable Sbugt.....ts deux fois diff?renci?

pacf(Sbugt.....ts)#### Corr?logramme partielle de la variable Sbugt.....ts

pacf(dSbugt.....ts)#### Corr?logramme partielle de la variable dSbugt.....ts utilis? pour le choix du lag optimal de la variable Sbugt.....ts

pacf(ddSbugt.....ts)#### Corr?logramme partielle de la variable ddSbugt.....ts utilis? pour le choix du lag optimal de la variable dSbugt.....ts

plot.ts(Sbugt.....ts,col="blue",main="solde budg?taire",xlab="ann?e",ylab="solde budg?taire")#### Graphe de la variable Sbugt.....ts

plot.ts(dSbugt.....ts,col="blue",main="dsolde budg?taire",xlab="ann?e",ylab="dsolde budg?taire")#### Graphe de la variable dSbugt.....ts

plot.ts(ddSbugt.....ts,col="blue",main="ddsolde budg?taire",xlab="ann?e",ylab="ddsolde budg?taire")#### Graphe de la variable ddSbugt.....ts

model3Sbugt.....ts <- ur.df(Sbugt.....ts, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable Sbugt.....ts avec un retard optimal de 0

summary(model3Sbugt.....ts)#### Affichage r?sultats du model3Sbugt.....ts

model2Sbugt.....ts <- ur.df(Sbugt.....ts, type = "drift", lags = 0)#### Test de dickey fuller sans tendance avec constante sur la variable Sbugt.....ts avec un retard optimal de 0

summary(model2Sbugt.....ts)#### Affichage r?sultats du model2Sbugt.....ts

trend <- as.numeric(time(Sbugt.....ts))#### Extraction de la tendance de la s?rie

RegSbugt.....ts <- lm(Sbugt.....ts~trend)#### Regression de la s?rie sur sa tendance 

summary.lm(RegSbugt.....ts)#### affichage r?sultat de la regression

Sbugt.....stts <- RegSbugt.....ts$residuals#### extraction du r?sidus de la regression

model3Sbugt.....stts <- ur.df(Sbugt.....stts, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable Sbugt.....stts avec un retard optimal de 0

summary(model3Sbugt.....stts)#### Affichage r?sultats du model3Sbugt.....stts

model2Sbugt.....stts <- ur.df(Sbugt.....stts, type = "drift", lags = 0)#### Test de dickey fuller sans tendance avec constante sur la variable Sbugt.....stts avec un retard optimal de 0

summary(model2Sbugt.....stts)#### Affichage r?sultats du model2Sbugt.....stts

model1Sbugt.....stts <- ur.df(Sbugt.....stts, type = "none", lags = 0)#### Test de dickey fuller sans tendance sans constante sur la variable Sbugt.....stts avec un retard optimal de 0

summary(model1Sbugt.....stts)#### Affichage r?sultats du model1Sbugt.....stts

#################################### Test de coint?gration de Johansen

Mod?le1.2 <- data.frame(SPEts,Inva.....ts,PIBa.....ts ,Taux.d.ouverturets, inflation..d?flacteur.du.PIB....annuel.ts, Tchants,Spots, Sbugt.....stts) ##### Regroupement variable d'ordre d'int?gration 1

Mod?le1.2ts <- ts(Mod?le1.2,start = 1980, end = 2017, frequency = 1)#### Cr?ation de la base de s?rie temporelle

jotest <- ca.jo(Mod?le1.2ts, type = "eigen", ecdet= "const", K=2)#### Coint?gration de Johansen suivant le crit?re valeur propre minimale

summary(jotest)#### R?sultat du test

jotest2 <- ca.jo(Mod?le1.2ts, type = "trace", ecdet= "const", K=2)#### Coint?gration de Johansen suivant le crit?re trace

summary(jotest2)#### R?sultat du test

#### Estimation MCO Long terme

MCElt <- lm(SPEts ~ Inva.....ts + PIBa.....ts + Taux.d.ouverturets + inflation..d?flacteur.du.PIB....annuel.ts + Tchants + Spots + Sbugt.....stts)

summary.lm(MCElt)#### R?sultat du mod?le 

res <- MCElt$residuals#### R?sidus du mod?le

########### TEST DE VALIDATION MODELE MCElt

##### TEST Normalit?

jarque.bera.test(resid(MCElt))

##### TEST D'autocorr?lation des erreurs

bgtest(MCElt)

##### TEST D'H?t?rosc?dascTICIT?

bptest(MCElt)

##### TEST De Stabilit?

cusum(resid(MCElt))

RMCElag1<-Lag(as.vector(res), k=1) #### r?sidus retarde d'une p?riode

Res <-RMCElag1[-1]#### cr?ation de rmce = RMCElag1[-1]( la premi?re colonne de RMCElag1 ? ?t? enlever)

inflation..d?flacteur.du.PIB....annuel.tslag1 <- Lag(as.vector(inflation..d?flacteur.du.PIB....annuel.ts), k=1) #### inflation..d?flacteur.du.PIB....annuel.ts retarde d'une p?riode

inflation..d?flacteur.du.PIB....annuel.tsmce <-inflation..d?flacteur.du.PIB....annuel.tslag1[-1]#### inflation..d?flacteur.du.PIB....annuel.tslag1 moins la premi?re colonne

Sbugt.....tslag1 <- Lag(as.vector(Sbugt.....stts), k=1)#### Sbugt.....stts retarde d'une p?riode

Sbugt.....tsmce <-Sbugt.....tslag1[-1]#### Sbugt.....tslag1 moins la premi?re colonne

######Creation dummy 94

dummy <- data.frame(year = 1981:2017)

dummy94 <- as.numeric(dummy== 1994)

#### Estimation MCO Court terme

MCEct <- lm(dSPEts ~ dInva.....ts + dPIBa.....ts + dTaux.d.ouverturets + inflation..d?flacteur.du.PIB....annuel.tsmce + dTchants + dSpots + Sbugt.....tsmce + Res + dummy94) 

summary.lm(MCEct)

########### TEST DE VALIDATION MODELE MCElt

##### TEST Normalit?

jarque.bera.test(resid(MCEct))

##### TEST D'autocorr?lation des erreurs

bgtest(MCEct)

##### TEST D'H?t?rosc?dascTICIT?

bptest(MCEct)

##### TEST De Stabilit?

cusum(resid(MCEct))

######################################################### MODELE 2

Mod?le2 <-read.table("clipboard", header=TRUE, dec=",", sep="\t", row.names = 1)

attach(Mod?le2)#### acc?s au variable de la base

names(Mod?le2)#### nom des variable de la base

Mod?le2.ts <- ts(Mod?le2,start = 1980, end = 2017, frequency = 1)#### transformation base en base de s?rie temporelle

plot.ts(Mod?le2.ts)#### courbe des variables du mod?le en fonction du temps

####1-Test d'ADF sur Le PIB (Produit Int?rieur Brut)

ln.PIB.ts <- ts(ln.PIB.,start = 1980, end = 2017, frequency = 1)#### transformation variable ln.PIB. en s?rie temporelle ln.PIB.ts

dln.PIB.ts <- diff(ln.PIB.ts)#### Cr?ation de la variable ln.PIB.ts diff?rienci?

ddln.PIB.ts <- diff(dln.PIB.ts)#### Cr?ation de la variable ln.PIB.ts deux fois diff?renci?

pacf(ln.PIB.ts)#### Corr?logramme partielle de la variable ln.PIB.ts

pacf(dln.PIB.ts)#### Corr?logramme partielle de la variable dln.PIB.ts utilis? pour le choix du lag optimal de la variable ln.PIB.ts

pacf(ddln.PIB.ts)#### Corr?logramme partielle de la variable ddln.PIB.ts utilis? pour le choix du lag optimal de la variable dln.PIB.ts

plot.ts(ln.PIB.ts ,col="blue",main="ln.PIB.ts",xlab="ann?e",ylab="ln.PIB.ts")#### Graphe de la variable ln.PIB.ts

plot.ts(dln.PIB.ts ,col="blue",main="dln.PIB.ts",xlab="ann?e",ylab="dln.PIB.ts")#### Graphe de la variable dln.PIB.ts

model3ln.PIB.ts <- ur.df(ln.PIB.ts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable ln.PIB.ts avec un retard optimal de 1

summary(model3ln.PIB.ts)#### Affichage r?sultats du model3ln.PIB.ts

model3dln.PIB.ts <- ur.df(dln.PIB.ts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable dln.PIB.ts avec un retard optimal de 1

summary(model3dln.PIB.ts)#### Affichage r?sultats du model3dln.PIB.ts

model2dln.PIB.ts <- ur.df(dln.PIB.ts, type = "drift", lags = 1)#### Test de dickey fuller sans tendance avec constante sur la variable dln.PIB.ts avec un retard optimal de 1

summary(model2dln.PIB.ts)#### Affichage r?sultats du model2dln.PIB.ts

#### 2-Test d'ADF sur capital humain( taux de scolarisation)

Human....ts <- ts(Human....,start = 1980, end = 2017, frequency = 1)#### transformation variable Human.... en s?rie temporelle Human....ts

dHuman....ts<- diff(Human....ts)#### Cr?ation de la variable Human....ts diff?rienci?

ddHuman....ts<- diff(dHuman....ts)#### Cr?ation de la variable Human....ts deux fois diff?renci?

pacf(Human....ts)#### Corr?logramme partielle de la variable Human....ts

pacf(dHuman....ts)#### Corr?logramme partielle de la variable dHuman....ts utilis? pour le choix du lag optimal de la variable Human....ts

pacf(ddHuman....ts)#### Corr?logramme partielle de la variable ddHuman....ts utilis? pour le choix du lag optimal de la variable dHuman....ts

plot.ts(Human....ts ,col="blue",main="Human....ts",xlab="ann?e",ylab="Human....ts")#### Graphe de la variable Human....ts

plot.ts(dHuman....ts ,col="blue",main="dHuman....ts",xlab="ann?e",ylab="dHuman....ts")#### Graphe de la variable dHuman....ts

model3Human....ts<- ur.df(Human....ts, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable Human....ts avec un retard optimal de 0

summary(model3Human....ts)#### Affichage r?sultats du model3Human....ts

model3dHuman....ts<- ur.df(dHuman....ts, type = "trend", lags = 4)#### Test de dickey fuller avec tendance avec constante sur la variable dHuman....ts avec un retard optimal de 4

summary(model3dHuman....ts)#### Affichage r?sultats du model3dHuman....ts

model2dHuman....ts<- ur.df(dHuman....ts, type = "drift", lags = 4)#### Test de dickey fuller sans tendance avec constante sur la variable dHuman....ts avec un retard optimal de 4

summary(model2dHuman....ts)#### Affichage r?sultats du model2dHuman....ts

model1dHuman....ts<- ur.df(dHuman....ts, type = "none", lags = 4)#### Test de dickey fuller sans tendance sans constante sur la variable dHuman....ts avec un retard optimal de 4

summary(model1dHuman....ts)#### Affichage r?sultats du model1dHuman....ts

#### 3-Test d'ADF sur La FBcF(Formation brute de capital fixe)

ln.FBCF.ts <- ts(ln.FBCF.,start = 1980, end = 2017, frequency = 1)#### transformation variable ln.FBCF. en s?rie temporelle ln.FBCF.ts 

dln.FBCF.ts  <- diff(ln.FBCF.ts )#### Cr?ation de la variable ln.FBCF.ts  diff?rienci?

ddln.FBCF.ts  <- diff(dln.FBCF.ts )#### Cr?ation de la variable ln.FBCF.ts  deux fois diff?renci?

pacf(ln.FBCF.ts)#### Corr?logramme partielle de la variable ln.FBCF.ts 

pacf(dln.FBCF.ts)#### Corr?logramme partielle de la variable dln.FBCF.ts utilis? pour le choix du lag optimal de la variable ln.FBCF.ts

pacf(ddln.FBCF.ts)#### Corr?logramme partielle de la variable ddln.FBCF.ts utilis? pour le choix du lag optimal de la variable dln.FBCF.ts

plot.ts(ln.FBCF.ts ,col="blue",main="ln.FBCF.ts",xlab="ann?e",ylab="ln.FBCF.ts")#### Graphe de la variable ln.FBCF.ts

plot.ts(dln.FBCF.ts ,col="blue",main="dln.FBCF.ts",xlab="ann?e",ylab="dln.FBCF.ts")#### Graphe de la variable ln.FBCF.ts

model3ln.FBCF.ts <- ur.df(ln.FBCF.ts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable ln.FBCF.ts avec un retard optimal de 1

summary(model3ln.FBCF.ts)#### Affichage r?sultats du model3ln.FBCF.ts

model3dln.FBCF.ts<- ur.df(dln.FBCF.ts, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable dln.FBCF.ts avec un retard optimal de 2

summary(model3dln.FBCF.ts)#### Affichage r?sultats du model3dln.FBCF.ts

model2dln.FBCF.ts<- ur.df(dln.FBCF.ts, type = "drift", lags = 2)#### Test de dickey fuller sans tendance avec constante sur la variable dln.FBCF.ts avec un retard optimal de 2

summary(model2dln.FBCF.ts)#### Affichage r?sultats du model2dln.FBCF.ts

#### 4-Test d'ADF sur la Population ?g?e de 15 ? 64 ans 

Pop....ts <- ts(Pop..15.?.64.ans.,start = 1980, end = 2017, frequency = 1)#### transformation variable Pop..15.?.64.ans. en s?rie temporelle Pop....ts

dPop....ts <- diff(Pop....ts)#### Cr?ation de la variable Pop....ts diff?rienci?

ddPop....ts <- diff(dPop....ts)#### Cr?ation de la variable Pop....ts deux fois diff?renci?

pacf(Pop....ts)#### Corr?logramme partielle de la variable Pop....ts

pacf(dPop....ts)#### Corr?logramme partielle de la variable dPop....ts utilis? pour le choix du lag optimal de la variable Pop....ts

pacf(ddPop....ts)#### Corr?logramme partielle de la variable ddPop....ts utilis? pour le choix du lag optimal de la variable dPop....ts

plot.ts(Pop....ts ,col="blue",main="Pop....ts",xlab="ann?e",ylab="Pop....ts")#### Graphe de la variable Pop....ts

plot.ts(dPop....ts ,col="blue",main="dPop....ts",xlab="ann?e",ylab="dPop....ts")#### Graphe de la variable dPop....ts

model3Pop....ts<- ur.df(Pop....ts, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable Pop....ts avec un retard optimal de 0

summary(model3Pop....ts)#### Affichage r?sultats du model3Pop....ts

model3dPop....ts<- ur.df(dPop....ts, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable dPop....ts avec un retard optimal de 1

summary(model3dPop....ts)#### Affichage r?sultats du model3dPop....ts

model2dPop....ts<- ur.df(dPop....ts, type = "drift", lags = 1)#### Test de dickey fuller sans tendance avec constante sur la variable dPop....ts avec un retard optimal de 1

summary(model2dPop....ts)#### Affichage r?sultats du model2dPop....ts

######################################################### Test de coint?gration de Johansen

Mod?le2.2 <- data.frame(ln.PIB.ts,SPEts,Human....ts,Spots,ln.FBCF.ts, Pop....ts, Taux.d.ouverturets) ##### Regroupement variable d'ordre d'int?gration 1

Mod?le2.2ts <- ts(Mod?le2.2,start = 1980, end = 2017, frequency = 1)#### Transformation de la base en base de s?rie temporelle

jotest3 <- ca.jo(Mod?le2.2ts, type = "eigen", ecdet= "const", K=2)#### Test de coint?gration de Johansen suivant le crit?re valeur propre minimale

summary(jotest3)#### R?sultat du Test

jotest4 <- ca.jo(Mod?le2.2ts, type = "trace", ecdet= "const", K=2)#### Test coint?gration de Johansen suivant le crit?re trace

summary(jotest4)#### R?sultat du Test
 
######Creation dummy 09

dummy <- data.frame(year = 1980:2017)

dummy09 <- as.numeric(dummy== 2009)

#### Estimation MCO Long terme (MCElt2.1)

MCElt2.1 <- lm(ln.PIB.ts ~ SPEts + Human....ts + Spots+ ln.FBCF.ts + Pop....ts + Taux.d.ouverturets + dummy09)

summary.lm(MCElt2.1)

########### TEST DE VALIDATION MODELE MCElt2.1

##### TEST Normalit?

jarque.bera.test(resid(MCElt2.1))

##### TEST D'autocorr?lation des erreurs

bgtest(MCElt2.1)

##### TEST D'H?t?rosc?dascTICIT?

bptest(MCElt2.1)

##### TEST De Stabilit?

cusum(resid(MCElt2.1))

res2 <- MCElt2.1$residuals#### Extraction du r?sidus 2 du mod?le MCElt2.1

dres2 <- diff(res2)#### Cr?ation de la variable res2 diff?rienci?

pacf(res2)#### Corr?logramme partielle de la variable res2

pacf(dres2)#### Corr?logramme partielle de la variable dres2 utilis? pour le choix du lag optimal de la variable res2

model3res2 <- ur.df(res2, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable res2 avec un retard optimal de 0

summary(model3res2)#### Affichage r?sultats du model3res2

model2res2 <- ur.df(res2, type = "drift", lags = 0)#### Test de dickey fuller sans tendance avec constante sur la variable res2 avec un retard optimal de 0

summary(model2res2)#### Affichage r?sultats du model2res2

model1res2 <- ur.df(res2, type = "none", lags = 0)#### Test de dickey fuller sans tendance sans constante sur la variable res2 avec un retard optimal de 0

summary(model1res2)#### Affichage r?sultats du model1res2

RMCE2lag1<-Lag(as.vector(res2), k=1) #### r?sidus retarde d'une p?riode

Res2 <-RMCE2lag1[-1]#### cr?ation de rmce = RMCElag1[-1]( la premi?re colonne de RMCElag1 ? ?t? enlever)

###### Cr?ation dummy

dummy2 <- data.frame(year = 1981:2017)

dummy89 <- as.numeric(dummy2== 1989)

#### Estimation MCO Court terme MCEct2

MCEct2 <- lm(dln.PIB.ts ~ dSPEts + dHuman....ts +dSpots + dln.FBCF.ts + dPop....ts + dTaux.d.ouverturets + dummy89 +  Res2)

summary.lm(MCEct2)

########### TEST DE VALIDATION MODELE MCEct2

##### TEST Normalit?

jarque.bera.test(resid(MCEct2))

##### TEST D'autocorr?lation des erreurs

bgtest(MCEct2)

##### TEST D'H?t?rosc?dascTICIT?

bptest(MCEct2)

##### TEST De Stabilit?

cusum(resid(MCEct2))

#### correction Mod?le (Mod?le cochrane)

MCEct3 <- cochrane.orcutt(MCEct2) #### Mod?le MCEct2 corrig? avec cochrane

summary.orcutt(MCEct3)

bgtest(MCEct3)

bptest(MCEct3)

jarque.bera.test(resid(MCEct3))

cusum(resid(MCEct3))

Mcect4 <-cochrane.orcutt(MCEct3)

summary.orcutt(Mcect4)

bgtest(Mcect4)

bptest(Mcect4)

jarque.bera.test(resid(Mcect4))

cusum(resid(Mcect4))


####TEst SPE AND PIB

SPEcarr? <- SPEts^2

Mod?le3 <-read.table("clipboard", header=TRUE, dec=",", sep="\t", row.names = 1)#### Importation de la base (Copier la base estimation Berth?lemy dans excel puis ex?cuter la commande)

attach(Mod?le3)#### acc?s au variable de la base

names(Mod?le3)#### nom des variable de la base

Mod?le3.ts <- ts(Mod?le3,start = 1980, end = 2017, frequency = 1)#### transformation base en base de s?rie temporelle

plot.ts(Mod?le3.ts)#### courbe des variables du mod?le en fonction du temps

##### Estimation Pib, SPE et Pop.total

REg <- lm(ln.PIB.ts ~ SPEts + SPEcarr? + Pop.total)

summary(REg)


###### Graphique 1(Courbe de l'Indice de sp?cialisation)

plot.ts(SPEts ,col="blue",xlab="ann?e",ylab="SPEts", xlim = c(1980,2016), ylim = c(0,1))#### Graphique Indice de Sp?cialisation

###### Graphique 2 (Courbe de l'indice de sp?cialisation et du PIB)

plot(ln.PIB., SPE, type = "l",ylim = c(0,1))

###### Graphique 3 (Evolution des exportations agricoles)

Mod?le4 <-read.table("clipboard", header=TRUE, dec=",", sep="\t", row.names = 1)#### Importation de la base (Copier la base Fr?q Expor prod agricole dans excel puis ex?cuter la commande)

attach(Mod?le4)#### acc?s au variable de la base

names(Mod?le4)#### nom des variable de la base

Mod?le4x <- as.matrix(Mod?le4)##### transformation de la base mod?le 4 en matrix mod?le 4x

colors = c("blue","orange","red","yellow","green", "cyan", "darkblue")#### S?lection des couleurs

par(xpd= T, mar= c(4,4,2,9))##### R?glagle Taille de la figure

barplot(Mod?le4x , col= colors, xlab = "Ann?e", ylab = "Pourcentage", space = 0,1) #### figure en barre empil?e

legend(locator(1), c("Ananas", "Coton", "Ma?s", "Riz","Palmiers ? huile", "Manioc", "Anarcade"), fill = colors)### Ins?rer l?gende sur la figure


#### M?thode d'Imputation utilis? pour les donn?es manquantes

Mod?le0 <- read.table("clipboard", header=TRUE, dec=",", sep="\t", row.names = 1)#### Importation de la base (Copier la base du mod?le 1(valeur manquante) dans excel comme exemple puis ex?cuter la commande)

attach(Mod?le0)#### acc?s au variable de la base

names(Mod?le0)#### nom des variable de la base

library("mice") #### Chargement package mice

md.pattern(Mod?le0)#### Statistique valeur manquante 

imp <- mice(Mod?le0)#### Imputation par d?faut

imp#### affichage r?sultat imputation

r1 <- complete(imp, action = 1L)#### generation r1 avec imputation 1 

r2 <- complete(imp, action = 2L)#### generation r2 avec imputation 2

r3 <- complete(imp, action = 3L)#### generation r3 avec imputation 3

r4 <- complete(imp, action = 4L)#### generation r4 avec imputation 4

r5 <- complete(imp, action = 5L)#### generation r5 avec imputation 5

Mod?le0final <- ((r1+r2+r3+r4+r5)/5)##### cr?ation modele final moyenne des 5 mod?le imput?

row.names(Mod?le0final) <- row.names(Mod?le0)##### colonne 0 mod?le1final = colonne p?riode mod?le1

attach(Mod?le0final)#### acc?s au variable de la base

names(Mod?le0final)#### nom des variable de la base

view(Mod?le0final)

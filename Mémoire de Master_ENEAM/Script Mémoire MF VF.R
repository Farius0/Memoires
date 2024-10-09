###### Analyse des données du mémoire ( Santé, Inégalité et Croissance économique)


###### Importation de la base de données

Modèle1 <- read.table("clipboard", header=TRUE, dec=",", sep="\t", row.names = 1)#### Importation de la base (Copier la base du mod?le 1 Final dans excel puis ex?cuter la commande)

attach(Modèle1)#### acc?s au variable de la base

names(Modèle1)#### nom des variable de la base


###### Bibliothèque des packages

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

library(knitr) #for kable() 

library(forecast)

library(zoo) # for time series functions (not much used here) 

library(pdfetch) # for retrieving data (just mentioned here) 

library(lmtest)  #for  ?coeftest()? and  ?bptest()?. 

library(broom) #for ?glance(?) and ?tidy()?

library(PoEdata) #for PoE4 datasets

library(sandwich)

library(dynlm) #for the ?dynlm()? function 

library(nlWaldTest) # for the ?nlWaldtest()? function

library(dynamac)

library(TSstudio)

library(aod)

library(dLagM)

library(performance)

library("GGally")

library(base)

library(nlme)

library(ARDL)



############################################ Stationnarité des variables(TEST ADF)

Modele.ts <- ts(Modèle1,start = 1985, end = 2021, frequency = 1)#### transformation de la base en base de s?rie temporelle

plot.ts(Modele.ts[,c(1:10)], col = "blue")#### courbe des variables du mod?le en fonction du temps

plot.ts(Modele.ts[,c(11:20)], col = "blue")#### courbe des variables du mod?le en fonction du temps

plot.ts(Modele.ts[,c(21:30)], col = "blue")#### courbe des variables du mod?le en fonction du temps

plot.ts(Modele.ts[,c(31:40)], col = "blue")#### courbe des variables du mod?le en fonction du temps

plot.ts(Modele.ts[,c(41:50)], col = "blue")#### courbe des variables du mod?le en fonction du temps


#### 1-Test d'ADF sur Logarithme Népérien du Produit Intérieur Brut Réel (LN_PIB)

LN_PIB <- ts(LN_PIB,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dLN_PIB <- diff(LN_PIB)#### Cr?ation de la variable diff?rienci?

ddLN_PIB <- diff(dLN_PIB)#### Cr?ation de la variable deux fois diff?renci?

pacf(LN_PIB)#### Corr?logramme partielle de la variable

pacf(dLN_PIB)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddLN_PIB)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

LagTest_LN_PIB <- dynlm(LN_PIB~ L(LN_PIB)+L(LN_PIB,2)+L(LN_PIB,3)+L(LN_PIB,4))

LagTest_dLN_PIB <- dynlm(dLN_PIB~L(dLN_PIB)+L(dLN_PIB,2)+L(dLN_PIB,3)+L(dLN_PIB,4))

kable(tidy(LagTest_LN_PIB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dLN_PIB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

plot.ts(LN_PIB ,col="blue",main="LN_PIB",Xtitle="Année",ylab="LN_PIB")#### Graphe de la variable

plot.ts(dLN_PIB ,col="blue",main="dLN_PIB",xlab="Année",ylab="dLN_PIB")#### Graphe de la variable

plot.ts(ddLN_PIB ,col="blue",main="ddLN_PIB",xlab="Année",ylab="ddLN_PIB")#### Graphe de la variable

model3LN_PIB<- ur.df(LN_PIB, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_PIB)#### Affichage r?sultats du modele

trend <- as.numeric(time(LN_PIB))#### Extraction de la tendance de la s?rie

LN_PIBts <- lm(LN_PIB~trend)#### Regression de la s?rie sur sa tendance 

summary.lm(LN_PIBts)#### affichage r?sultat de la regression

LN_PIB_TS <- LN_PIBts$residuals#### extraction du r?sidus de la regression

model3LN_PIB_TS  <- ur.df(LN_PIB_TS, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_PIB_TS )#### Affichage r?sultats

model3dLN_PIB <- ur.df(dLN_PIB, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dLN_PIB)#### Affichage r?sultats

model2dLN_PIB <- ur.df(dLN_PIB, type = "drift", lags = 2)#### Test de dickey fuller sans tendance avec constante sur la variable

summary(model2dLN_PIB)#### Affichage r?sultats

pp.test(LN_PIB)

pp.test(dLN_PIB)

kpss.test(LN_PIB)

kpss.test(dLN_PIB)


#### 2-Test d'ADF sur Logarithme Népérien des Dépenses en Médicaments et matériels (LN_DEP_SANT)

LN_DEP_SANT <- ts(LN_DEP_SANT,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dLN_DEP_SANT <- diff(LN_DEP_SANT)#### Cr?ation de la variable diff?rienci?

ddLN_DEP_SANT <- diff(dLN_DEP_SANT)#### Cr?ation de la variable deux fois diff?renci?

pacf(LN_DEP_SANT)#### Corr?logramme partielle de la variable

pacf(dLN_DEP_SANT)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddLN_DEP_SANT)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(LN_DEP_SANT ,col="blue",main="LN_DEP_SANT",xlab="ann?e",ylab="LN_DEP_SANT")#### Graphe de la variable

plot.ts(dLN_DEP_SANT ,col="blue",main="dLN_DEP_SANT",xlab="ann?e",ylab="dLN_DEP_SANT")#### Graphe de la variable

plot.ts(ddLN_DEP_SANT ,col="blue",main="ddLN_DEP_SANT",xlab="ann?e",ylab="ddLN_DEP_SANT")#### Graphe de la variable

LagTest_LN_DEP_SANT <- dynlm(LN_DEP_SANT ~ L(LN_DEP_SANT)+L(LN_DEP_SANT,2)+L(LN_DEP_SANT,3)+L(LN_DEP_SANT,4))

LagTest_dLN_DEP_SANT <- dynlm(dLN_DEP_SANT ~ L(dLN_DEP_SANT)+L(dLN_DEP_SANT,2)+L(dLN_DEP_SANT,3)+L(dLN_DEP_SANT,4))

kable(tidy(LagTest_LN_DEP_SANT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dLN_DEP_SANT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3LN_DEP_SANT<- ur.df(LN_DEP_SANT, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_DEP_SANT)#### Affichage r?sultats du modele

model2LN_DEP_SANT<- ur.df(LN_DEP_SANT, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2LN_DEP_SANT)#### Affichage r?sultats du modele

model1LN_DEP_SANT<- ur.df(LN_DEP_SANT, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1LN_DEP_SANT)#### Affichage r?sultats du modele

model3dLN_DEP_SANT<- ur.df(dLN_DEP_SANT, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dLN_DEP_SANT)#### Affichage r?sultats du modele

trend1 <- as.numeric(time(dLN_DEP_SANT))#### Extraction de la tendance de la s?rie

dLN_DEP_SANTts <- lm(dLN_DEP_SANT~trend1)#### Regression de la s?rie sur sa tendance 

summary.lm(dLN_DEP_SANTts)#### affichage r?sultat de la regression

dLN_DEP_SANT_TS <- dLN_DEP_SANTts$residuals#### extraction du r?sidus de la regression

pacf(diff(dLN_DEP_SANT_TS))

model3dLN_DEP_SANT_TS<- ur.df(dLN_DEP_SANT_TS, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dLN_DEP_SANT_TS)#### Affichage r?sultats du modele

model2dLN_DEP_SANT_TS<- ur.df(dLN_DEP_SANT_TS, type = "drift", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dLN_DEP_SANT_TS)#### Affichage r?sultats du modele

model1dLN_DEP_SANT_TS<- ur.df(dLN_DEP_SANT_TS, type = "none", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1dLN_DEP_SANT_TS)#### Affichage r?sultats du modele

pp.test(LN_DEP_SANT)

pp.test(dLN_DEP_SANT)

kpss.test(LN_DEP_SANT)

kpss.test(dLN_DEP_SANT)


#### 3-Test d'ADF sur Logarithme Népérien du Produit Intérieur Brut Réel par Habitant (LN_PIB_HAB)

LN_PIB_HAB <- ts(LN_PIB_HAB,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dLN_PIB_HAB <- diff(LN_PIB_HAB)#### Cr?ation de la variable diff?rienci?

ddLN_PIB_HAB <- diff(dLN_PIB_HAB)#### Cr?ation de la variable deux fois diff?renci?

pacf(LN_PIB_HAB)#### Corr?logramme partielle de la variable

pacf(dLN_PIB_HAB)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddLN_PIB_HAB)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(LN_PIB_HAB,col="blue",main="LN_PIB_HAB",xlab="ann?e",ylab="LN_PIB_HAB")#### Graphe de la variable

plot.ts(dLN_PIB_HAB,col="blue",main="dLN_PIB_HAB",xlab="ann?e",ylab="dLN_PIB_HAB")#### Graphe de la variable

plot.ts(ddLN_PIB_HAB ,col="blue",main="ddLN_PIB_HAB",xlab="ann?e",ylab="ddLN_PIB_HAB")#### Graphe de la variable

LagTest_LN_PIB_HAB  <- dynlm(LN_PIB_HAB ~ L(LN_PIB_HAB )+L(LN_PIB_HAB ,2)+L(LN_PIB_HAB ,3)+L(LN_PIB_HAB ,4))

LagTest_dLN_PIB_HAB  <- dynlm(dLN_PIB_HAB  ~ L(dLN_PIB_HAB )+L(dLN_PIB_HAB ,2)+L(dLN_PIB_HAB ,3)+L(dLN_PIB_HAB ,4))

kable(tidy(LagTest_LN_PIB_HAB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dLN_PIB_HAB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3LN_PIB_HAB<- ur.df(LN_PIB_HAB, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_PIB_HAB)#### Affichage r?sultats du modele

model3LN_PIB_HAB2 <- ur.pp(LN_PIB_HAB, type = "Z-tau",model ="constant"  ,lags = "short")#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_PIB_HAB2)#### Affichage r?sultats du modele

model3dLN_PIB_HAB<- ur.df(dLN_PIB_HAB, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dLN_PIB_HAB)#### Affichage r?sultats du modele

model2dLN_PIB_HAB<- ur.df(dLN_PIB_HAB, type = "drift", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dLN_PIB_HAB)#### Affichage r?sultats du modele

adf.test(LN_PIB_HAB)

pp.test(LN_PIB_HAB)

pp.test(dLN_PIB_HAB)

kpss.test(LN_PIB_HAB)

kpss.test(dLN_PIB_HAB)


#### 4-Test d'ADF sur Logarithme Népérien des Dépenses de Santé par Habitant (LN_DEP_SANT_HAB)

LN_DEP_SANT_HAB <- ts(LN_DEP_SANT_HAB,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dLN_DEP_SANT_HAB <- diff(LN_DEP_SANT_HAB)#### Cr?ation de la variable diff?rienci?

ddLN_DEP_SANT_HAB <- diff(dLN_DEP_SANT_HAB)#### Cr?ation de la variable deux fois diff?renci?

pacf(LN_DEP_SANT_HAB)#### Corr?logramme partielle de la variable

pacf(dLN_DEP_SANT_HAB)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddLN_DEP_SANT_HAB)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(LN_DEP_SANT_HAB ,col="blue",main="LN_DEP_SANT_HAB",xlab="ann?e",ylab="LN_DEP_SANT_HAB")#### Graphe de la variable

plot.ts(dLN_DEP_SANT_HAB ,col="blue",main="dLN_DEP_SANT_HAB",xlab="ann?e",ylab="dLN_DEP_SANT_HAB")#### Graphe de la variable

plot.ts(ddLN_DEP_SANT_HAB ,col="blue",main="ddLN_DEP_SANT_HAB",xlab="ann?e",ylab="ddLN_DEP_SANT_HAB")#### Graphe de la variable

LagTest_LN_DEP_SANT_HAB <- dynlm(LN_DEP_SANT_HAB ~ L(LN_DEP_SANT_HAB)+L(LN_DEP_SANT_HAB,2)+L(LN_DEP_SANT_HAB,3)+L(LN_DEP_SANT_HAB,4))

LagTest_dLN_DEP_SANT_HAB <- dynlm(dLN_DEP_SANT_HAB ~ L(dLN_DEP_SANT_HAB)+L(dLN_DEP_SANT_HAB,2)+L(dLN_DEP_SANT_HAB,3)+L(dLN_DEP_SANT_HAB,4))

kable(tidy(LagTest_LN_DEP_SANT_HAB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dLN_DEP_SANT_HAB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3LN_DEP_SANT_HAB<- ur.df(LN_DEP_SANT_HAB, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_DEP_SANT_HAB)#### Affichage r?sultats du modele

model2LN_DEP_SANT_HAB<- ur.df(LN_DEP_SANT_HAB, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2LN_DEP_SANT_HAB)#### Affichage r?sultats du modele

model1LN_DEP_SANT_HAB<- ur.df(LN_DEP_SANT_HAB, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1LN_DEP_SANT_HAB)#### Affichage r?sultats du modele

model3dLN_DEP_SANT_HAB<- ur.df(dLN_DEP_SANT_HAB, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dLN_DEP_SANT_HAB)#### Affichage r?sultats du modele

model2dLN_DEP_SANT_HAB<- ur.df(dLN_DEP_SANT_HAB, type = "drift", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dLN_DEP_SANT_HAB)#### Affichage r?sultats du modele

model1dLN_DEP_SANT_HAB<- ur.df(dLN_DEP_SANT_HAB, type = "none", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1dLN_DEP_SANT_HAB)#### Affichage r?sultats du modele

pp.test(LN_DEP_SANT_HAB)

pp.test(dLN_DEP_SANT_HAB)

kpss.test(LN_DEP_SANT_HAB)

kpss.test(dLN_DEP_SANT_HAB)

#### 5-Test d'ADF sur Logarithme Népérien du Produit Intérieur Brut Réel par Habitant (LN_PIB_HAB)

LN_PIB_HAB_ACTV <- ts(LN_PIB_HAB_ACTV,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dLN_PIB_HAB_ACTV <- diff(LN_PIB_HAB_ACTV)#### Cr?ation de la variable diff?rienci?

ddLN_PIB_HAB_ACTV <- diff(dLN_PIB_HAB_ACTV)#### Cr?ation de la variable deux fois diff?renci?

pacf(LN_PIB_HAB_ACTV)#### Corr?logramme partielle de la variable

pacf(dLN_PIB_HAB_ACTV)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddLN_PIB_HAB_ACTV)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(LN_PIB_HAB_ACTV,col="blue",main="LN_PIB_HAB_ACTV",xlab="ann?e",ylab="LN_PIB_HAB_ACTV")#### Graphe de la variable

plot.ts(dLN_PIB_HAB_ACTV,col="blue",main="dLN_PIB_HAB_ACTV",xlab="ann?e",ylab="dLN_PIB_HAB_ACTV")#### Graphe de la variable

plot.ts(ddLN_PIB_HAB_ACTV ,col="blue",main="ddLN_PIB_HAB_ACTV",xlab="ann?e",ylab="ddLN_PIB_HAB_ACTV")#### Graphe de la variable

LagTest_LN_PIB_HAB_ACTV  <- dynlm(LN_PIB_HAB_ACTV ~ L(LN_PIB_HAB_ACTV)+L(LN_PIB_HAB_ACTV ,2)+L(LN_PIB_HAB_ACTV,3)+L(LN_PIB_HAB_ACTV,4))

LagTest_dLN_PIB_HAB_ACTV  <- dynlm(dLN_PIB_HAB_ACTV  ~ L(dLN_PIB_HAB_ACTV )+L(dLN_PIB_HAB_ACTV ,2)+L(dLN_PIB_HAB_ACTV ,3)+L(dLN_PIB_HAB_ACTV ,4))

kable(tidy(LagTest_LN_PIB_HAB_ACTV),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dLN_PIB_HAB_ACTV),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3LN_PIB_HAB_ACTV <- ur.df(LN_PIB_HAB_ACTV, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_PIB_HAB_ACTV)#### Affichage r?sultats du modele

model3dLN_PIB_HAB_ACTV<- ur.df(dLN_PIB_HAB_ACTV, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dLN_PIB_HAB_ACTV)#### Affichage r?sultats du modele

model2dLN_PIB_HAB_ACTV<- ur.df(dLN_PIB_HAB_ACTV, type = "drift", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dLN_PIB_HAB_ACTV)#### Affichage r?sultats du modele

pp.test(LN_PIB_HAB_ACTV)

pp.test(dLN_PIB_HAB_ACTV)

kpss.test(LN_PIB_HAB_ACTV)

kpss.test(dLN_PIB_HAB_ACTV)


#### 6-Test d'ADF sur Mortalité Martenelle pour 100000 NV (MORT_MAT)

MORT_MAT <- ts(MORT_MAT,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dMORT_MAT <- diff(MORT_MAT)#### Cr?ation de la variable diff?rienci?

ddMORT_MAT <- diff(dMORT_MAT)#### Cr?ation de la variable deux fois diff?renci?

pacf(MORT_MAT)#### Corr?logramme partielle de la variable

pacf(dMORT_MAT)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddMORT_MAT)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(MORT_MAT,col="blue",main="MORT_MAT",xlab="ann?e",ylab="MORT_MAT")#### Graphe de la variable

plot.ts(dMORT_MAT,col="blue",main="MORT_MAT",xlab="ann?e",ylab="MORT_MAT")#### Graphe de la variable

plot.ts(ddMORT_MAT ,col="blue",main="MORT_MAT",xlab="ann?e",ylab="MORT_MAT")#### Graphe de la variable

LagTest_MORT_MAT <- dynlm(MORT_MAT~ L(MORT_MAT)+L(MORT_MAT,2)+L(MORT_MAT,3)+L(MORT_MAT,4))

LagTest_dMORT_MAT <- dynlm(dMORT_MAT~L(dMORT_MAT)+L(dMORT_MAT,2)+L(dMORT_MAT,3)+L(dMORT_MAT,4))

kable(tidy(LagTest_MORT_MAT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dMORT_MAT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3MORT_MAT<- ur.df(MORT_MAT, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3MORT_MAT)#### Affichage r?sultats du modele

model2MORT_MAT<- ur.df(MORT_MAT, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2MORT_MAT)#### Affichage r?sultats du modele

adf.test(MORT_MAT)

pp.test(dMORT_MAT)

pp.test(dMORT_MAT)

kpss.test(MORT_MAT)


#### 7-Test d'ADF sur Espérance de vie à la naissance en années (ESP_VIE)

ESP_VIE <- ts(ESP_VIE,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dESP_VIE <- diff(ESP_VIE)#### Cr?ation de la variable diff?rienci?

ddESP_VIE <- diff(dESP_VIE)#### Cr?ation de la variable deux fois diff?renci?

pacf(ESP_VIE)#### Corr?logramme partielle de la variable

pacf(dESP_VIE)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddESP_VIE)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(ESP_VIE,col="blue",main="ESP_VIE",xlab="ann?e",ylab="ESP_VIE")#### Graphe de la variable

plot.ts(dESP_VIE,col="blue",main="ESP_VIE",xlab="ann?e",ylab="ESP_VIE")#### Graphe de la variable

plot.ts(ddESP_VIE ,col="blue",main="ESP_VIE",xlab="ann?e",ylab="ESP_VIE")#### Graphe de la variable

LagTest_ESP_VIE <- dynlm(ESP_VIE~ L(ESP_VIE)+L(ESP_VIE,2)+L(ESP_VIE,3)+L(ESP_VIE,4))

LagTest_dESP_VIE <- dynlm(dESP_VIE~L(dESP_VIE)+L(dESP_VIE,2)+L(dESP_VIE,3)+L(dESP_VIE,4))

kable(tidy(LagTest_ESP_VIE),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dESP_VIE),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3ESP_VIE<- ur.df(ESP_VIE, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3ESP_VIE)#### Affichage r?sultats du modele

model2ESP_VIE<- ur.df(ESP_VIE, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2ESP_VIE)#### Affichage r?sultats du modele

model1ESP_VIE<- ur.df(ESP_VIE, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1ESP_VIE)#### Affichage r?sultats du modele

pp.test(ESP_VIE)

pp.test(dESP_VIE)

kpss.test(ESP_VIE)


#### 8-Test d'ADF sur Taux de mortalité infantile pour 1000 NV (TAU_MORT_INF)

TAU_MOR_INF <- ts(TAU_MOR_INF,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTAU_MOR_INF <- diff(TAU_MOR_INF)#### Cr?ation de la variable diff?rienci?

ddTAU_MOR_INF <- diff(dTAU_MOR_INF)#### Cr?ation de la variable deux fois diff?renci?

pacf(TAU_MOR_INF)#### Corr?logramme partielle de la variable

pacf(dTAU_MOR_INF)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTAU_MOR_INF)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TAU_MOR_INF,col="blue",main="TAU_MORT_INF",xlab="ann?e",ylab="TAU_MORT_INF")#### Graphe de la variable

plot.ts(dTAU_MOR_INF,col="blue",main="TAU_MORT_INF",xlab="ann?e",ylab="TAU_MORT_INF")#### Graphe de la variable

plot.ts(ddTAU_MOR_INF ,col="blue",main="TAU_MORT_INF",xlab="ann?e",ylab="TAU_MORT_INF")#### Graphe de la variable

LagTest_TAU_MORT_INF <- dynlm(TAU_MOR_INF~ L(TAU_MOR_INF)+L(TAU_MOR_INF,2)+L(TAU_MOR_INF,3)+L(TAU_MOR_INF,4))

LagTest_dTAU_MORT_INF <- dynlm(dTAU_MOR_INF ~ L(dTAU_MOR_INF)+L(dTAU_MOR_INF,2)+L(dTAU_MOR_INF,3)+L(dTAU_MOR_INF,4))

kable(tidy(LagTest_TAU_MORT_INF),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTAU_MORT_INF),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TAU_MOR_INF <- ur.df(TAU_MOR_INF, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TAU_MOR_INF)#### Affichage r?sultats du modele

model2TAU_MOR_INF <- ur.df(TAU_MOR_INF, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TAU_MOR_INF)#### Affichage r?sultats du modele

model1TAU_MOR_INF <- ur.df(TAU_MOR_INF, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TAU_MOR_INF)#### Affichage r?sultats du modele

pp.test(TAU_MOR_INF)

kpss.test(TAU_MOR_INF)


#### 9-Test d'ADF sur Indice synthétique de Fécondité (IND_FEC)

IND_FEC <- ts(IND_FEC,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dIND_FEC <- diff(IND_FEC)#### Cr?ation de la variable diff?rienci?

ddIND_FEC <- diff(dIND_FEC)#### Cr?ation de la variable deux fois diff?renci?

pacf(IND_FEC)#### Corr?logramme partielle de la variable

pacf(dIND_FEC)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddIND_FEC)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(IND_FEC,col="blue",main="IND_FEC",xlab="ann?e",ylab="IND_FEC")#### Graphe de la variable

plot.ts(dIND_FEC,col="blue",main="IND_FEC",xlab="ann?e",ylab="IND_FEC")#### Graphe de la variable

plot.ts(ddIND_FEC ,col="blue",main="IND_FEC",xlab="ann?e",ylab="IND_FEC")#### Graphe de la variable

LagTest_IND_FEC <- dynlm(IND_FEC ~ L(IND_FEC)+L(IND_FEC,2)+L(IND_FEC,3)+L(IND_FEC,4))

LagTest_dIND_FEC <- dynlm(dIND_FEC ~ L(dIND_FEC)+L(dIND_FEC,2)+L(dIND_FEC,3)+L(dIND_FEC,4))

kable(tidy(LagTest_IND_FEC),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dIND_FEC),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3IND_FEC<- ur.df(IND_FEC, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3IND_FEC)#### Affichage r?sultats du modele

model2IND_FEC<- ur.df(IND_FEC, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2IND_FEC)#### Affichage r?sultats du modele

model1IND_FEC<- ur.df(IND_FEC, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1IND_FEC)#### Affichage r?sultats du modele

pp.test(IND_FEC)

pp.test(dIND_FEC)

kpss.test(IND_FEC)


#### 10-Test d'ADF sur Taux Brut de Natalité pour 1000 Pers (TB_NAT)

TB_NAT <- ts(TB_NAT,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTB_NAT <- diff(TB_NAT)#### Cr?ation de la variable diff?rienci?

ddTB_NAT <- diff(dTB_NAT)#### Cr?ation de la variable deux fois diff?renci?

pacf(TB_NAT)#### Corr?logramme partielle de la variable

pacf(dTB_NAT)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTB_NAT)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TB_NAT,col="blue",main="TB_NAT",xlab="ann?e",ylab="TB_NAT")#### Graphe de la variable

plot.ts(dTB_NAT,col="blue",main="TB_NAT",xlab="ann?e",ylab="TB_NAT")#### Graphe de la variable

plot.ts(ddTB_NAT ,col="blue",main="TB_NAT",xlab="ann?e",ylab="TB_NAT")#### Graphe de la variable

LagTest_TB_NAT <- dynlm(TB_NAT ~ L(TB_NAT)+L(TB_NAT,2)+L(TB_NAT,3)+L(TB_NAT,4))

LagTest_dTB_NAT <- dynlm(dTB_NAT ~ L(dTB_NAT)+L(dTB_NAT,2)+L(dTB_NAT,3)+L(dTB_NAT,4))

kable(tidy(LagTest_TB_NAT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTB_NAT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TB_NAT<- ur.df(TB_NAT, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TB_NAT)#### Affichage r?sultats du modele

model2TB_NAT<- ur.df(TB_NAT, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TB_NAT)#### Affichage r?sultats du modele

model1TB_NAT<- ur.df(TB_NAT, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TB_NAT)#### Affichage r?sultats du modele

model3dTB_NAT<- ur.df(dTB_NAT, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dTB_NAT)#### Affichage r?sultats du modele

model2dTB_NAT<- ur.df(dTB_NAT, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dTB_NAT)#### Affichage r?sultats du modele

model1dTB_NAT<- ur.df(dTB_NAT, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1dTB_NAT)#### Affichage r?sultats du modele

pp.test(TB_NAT)

pp.test(dTB_NAT)

kpss.test(TB_NAT)


#### 11-Test d'ADF sur Taux Brut de mortalité pour 1000 pers (TB_MORT)

TB_MORT <- ts(TB_MORT,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTB_MORT <- diff(TB_MORT)#### Cr?ation de la variable diff?rienci?

ddTB_MORT <- diff(dTB_MORT)#### Cr?ation de la variable deux fois diff?renci?

pacf(TB_MORT)#### Corr?logramme partielle de la variable

pacf(dTB_MORT)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTB_MORT)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TB_MORT,col="blue",main="TB_MORT",xlab="ann?e",ylab="TB_MORT")#### Graphe de la variable

plot.ts(dTB_MORT,col="blue",main="TB_MORT",xlab="ann?e",ylab="TB_MORT")#### Graphe de la variable

plot.ts(ddTB_MORT ,col="blue",main="TB_MORT",xlab="ann?e",ylab="TB_MORT")#### Graphe de la variable

LagTest_TB_MORT <- dynlm(TB_MORT ~ L(TB_MORT)+L(TB_MORT,2)+L(TB_MORT,3)+L(TB_MORT,4))

LagTest_dTB_MORT <- dynlm(dTB_MORT ~ L(dTB_MORT)+L(dTB_MORT,2)+L(dTB_MORT,3)+L(dTB_MORT,4))

kable(tidy(LagTest_TB_MORT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTB_MORT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TB_MORT<- ur.df(TB_MORT, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TB_MORT)#### Affichage r?sultats du modele

trend2 <- as.numeric(time(TB_MORT))#### Extraction de la tendance de la s?rie

TB_MORTts <- lm(TB_MORT~trend2)#### Regression de la s?rie sur sa tendance 

summary.lm(TB_MORTts)#### affichage r?sultat de la regression

TB_MORT_TS <- TB_MORTts$residuals#### extraction du r?sidus de la regression

pacf(diff(TB_MORT_TS))

model3TB_MORT_TS<- ur.df(TB_MORT_TS, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TB_MORT_TS)#### Affichage r?sultats du modele

model2TB_MORT_TS<- ur.df(TB_MORT_TS, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TB_MORT_TS)#### Affichage r?sultats du modele

model1TB_MORT_TS<- ur.df(TB_MORT_TS, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TB_MORT_TS)#### Affichage r?sultats du modele

pp.test(TB_MORT)

pp.test(TB_MORT_TS)

kpss.test(TB_MORT)

kpss.test(TB_MORT_TS)


#### 12-Test d'ADF sur Indicence Paludisme Simple + Grave pour 100 Pers (INC_PAL)

INC_PAL <- ts(INC_PAL,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dINC_PAL <- diff(INC_PAL)#### Cr?ation de la variable diff?rienci?

ddINC_PAL <- diff(dINC_PAL)#### Cr?ation de la variable deux fois diff?renci?

pacf(INC_PAL)#### Corr?logramme partielle de la variable

pacf(dINC_PAL)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddINC_PAL)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(INC_PAL,col="blue",main="INC_PAL",xlab="ann?e",ylab="INC_PAL")#### Graphe de la variable

plot.ts(dINC_PAL,col="blue",main="INC_PAL",xlab="ann?e",ylab="INC_PAL")#### Graphe de la variable

plot.ts(ddINC_PAL ,col="blue",main="INC_PAL",xlab="ann?e",ylab="INC_PAL")#### Graphe de la variable

LagTest_INC_PAL <- dynlm(INC_PAL ~ L(INC_PAL)+L(INC_PAL,2)+L(INC_PAL,3)+L(INC_PAL,4))

LagTest_dINC_PAL <- dynlm(dINC_PAL ~ L(dINC_PAL)+L(dINC_PAL,2)+L(dINC_PAL,3)+L(dINC_PAL,4))

kable(tidy(LagTest_INC_PAL),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dINC_PAL),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3INC_PAL <- ur.df(INC_PAL, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3INC_PAL)#### Affichage r?sultats du modele

trend4 <- as.numeric(time(INC_PAL))#### Extraction de la tendance de la s?rie

INC_PALts <- lm(INC_PAL~trend4)#### Regression de la s?rie sur sa tendance 

summary.lm(INC_PALts)#### affichage r?sultat de la regression

INC_PAL_TS <- INC_PALts$residuals#### extraction du r?sidus de la regression

pacf(diff(INC_PAL_TS))

model3INC_PAL_TS <- ur.df(INC_PAL_TS, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3INC_PAL_TS)#### Affichage r?sultats du modele

model2INC_PAL_TS <- ur.df(INC_PAL_TS, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2INC_PAL_TS)#### Affichage r?sultats du modele

model1INC_PAL_TS <- ur.df(INC_PAL_TS, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1INC_PAL_TS)#### Affichage r?sultats du modele

pp.test(INC_PAL_TS)

pp.test(INC_PAL)

kpss.test(INC_PAL)

kpss.test(INC_PAL_TS)


#### 13-Test d'ADF sur Incidence de l'Anémie pour 100 Pers (INC_ANE)

INC_ANE <- ts(INC_ANE,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dINC_ANE <- diff(INC_ANE)#### Cr?ation de la variable diff?rienci?

ddINC_ANE <- diff(dINC_ANE)#### Cr?ation de la variable deux fois diff?renci?

pacf(INC_ANE)#### Corr?logramme partielle de la variable

pacf(dINC_ANE)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddINC_ANE)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(INC_ANE,col="blue",main="INC_ANE",xlab="ann?e",ylab="INC_ANE")#### Graphe de la variable

plot.ts(dINC_ANE,col="blue",main="INC_ANE",xlab="ann?e",ylab="INC_ANE")#### Graphe de la variable

plot.ts(ddINC_ANE ,col="blue",main="INC_ANE",xlab="ann?e",ylab="INC_ANE")#### Graphe de la variable

LagTest_INC_ANE <- dynlm(INC_ANE ~ L(INC_ANE)+L(INC_ANE,2)+L(INC_ANE,3)+L(INC_ANE,4))

LagTest_dINC_ANE <- dynlm(dINC_ANE ~ L(dINC_ANE)+L(dINC_ANE,2)+L(dINC_ANE,3)+L(dINC_ANE,4))

kable(tidy(LagTest_INC_ANE),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dINC_ANE),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3INC_ANE <- ur.df(INC_ANE, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3INC_ANE)#### Affichage r?sultats du modele

model2INC_ANE <- ur.df(INC_ANE, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2INC_ANE)#### Affichage r?sultats du modele

model1INC_ANE <- ur.df(INC_ANE, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1INC_ANE)#### Affichage r?sultats du modele

model3dINC_ANE <- ur.df(dINC_ANE, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dINC_ANE)#### Affichage r?sultats du modele

model2dINC_ANE <- ur.df(dINC_ANE, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dINC_ANE)#### Affichage r?sultats du modele

model1dINC_ANE <- ur.df(dINC_ANE, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1dINC_ANE)#### Affichage r?sultats du modele

pp.test(INC_ANE)

kpss.test(INC_ANE)


#### 14-Test d'ADF sur Incidence des Infections Respiratoires Aiguës pour 100 Pers (INC_IRA) 

INC_IRA <- ts(INC_IRA,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dINC_IRA <- diff(INC_IRA)#### Cr?ation de la variable diff?rienci?

ddINC_IRA <- diff(dINC_IRA)#### Cr?ation de la variable deux fois diff?renci?

pacf(INC_IRA)#### Corr?logramme partielle de la variable

pacf(dINC_IRA)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddINC_IRA)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(INC_IRA,col="blue",main="INC_IRA",xlab="ann?e",ylab="INC_IRA")#### Graphe de la variable

plot.ts(dINC_IRA,col="blue",main="INC_IRA",xlab="ann?e",ylab="INC_IRA")#### Graphe de la variable

plot.ts(ddINC_IRA ,col="blue",main="INC_IRA",xlab="ann?e",ylab="INC_IRA")#### Graphe de la variable

LagTest_INC_IRA <- dynlm(INC_IRA ~ L(INC_IRA)+L(INC_IRA,2)+L(INC_IRA,3)+L(INC_IRA,4))

LagTest_dINC_IRA <- dynlm(dINC_IRA ~ L(dINC_IRA)+L(dINC_IRA,2)+L(dINC_IRA,3)+L(dINC_IRA,4))

kable(tidy(LagTest_INC_IRA),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dINC_IRA),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3INC_IRA <- ur.df(INC_IRA, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3INC_IRA)#### Affichage r?sultats du modele

model2INC_IRA <- ur.df(INC_IRA, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2INC_IRA)#### Affichage r?sultats du modele

pp.test(INC_IRA)

kpss.test(INC_IRA)


#### 15-Test d'ADF sur Pourcentage de Femme parmis les Agents Qualifiés (FEM_AGT_QUA)

FEM_AGT_QUA <- ts(FEM_AGT_QUA,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dFEM_AGT_QUA <- diff(FEM_AGT_QUA)#### Cr?ation de la variable diff?rienci?

ddFEM_AGT_QUA <- diff(dFEM_AGT_QUA)#### Cr?ation de la variable deux fois diff?renci?

pacf(FEM_AGT_QUA)#### Corr?logramme partielle de la variable

pacf(dFEM_AGT_QUA)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddFEM_AGT_QUA)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(FEM_AGT_QUA,col="blue",main="FEM_AGT_QUA",xlab="ann?e",ylab="FEM_AGT_QUA")#### Graphe de la variable

plot.ts(dFEM_AGT_QUA,col="blue",main="dFEM_AGT_QUA",xlab="ann?e",ylab="dFEM_AGT_QUA")#### Graphe de la variable

plot.ts(ddFEM_AGT_QUA ,col="blue",main="ddFEM_AGT_QUA",xlab="ann?e",ylab="ddFEM_AGT_QUA")#### Graphe de la variable

LagTest_FEM_AGT_QUA <- dynlm(FEM_AGT_QUA ~ L(FEM_AGT_QUA)+L(FEM_AGT_QUA,2)+L(FEM_AGT_QUA,3)+L(FEM_AGT_QUA,4))

LagTest_dFEM_AGT_QUA <- dynlm(dFEM_AGT_QUA ~ L(dFEM_AGT_QUA)+L(dFEM_AGT_QUA,2)+L(dFEM_AGT_QUA,3)+L(dFEM_AGT_QUA,4))

kable(tidy(LagTest_FEM_AGT_QUA),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dFEM_AGT_QUA),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3FEM_AGT_QUA <- ur.df(FEM_AGT_QUA, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3FEM_AGT_QUA) #### Affichage r?sultats du modele

trend6 <- as.numeric(time(FEM_AGT_QUA))#### Extraction de la tendance de la s?rie

FEM_AGT_QUAts <- lm(FEM_AGT_QUA~trend6)#### Regression de la s?rie sur sa tendance 

summary.lm(FEM_AGT_QUAts)#### affichage r?sultat de la regression

FEM_AGT_QUA_TS <- FEM_AGT_QUAts$residuals#### extraction du r?sidus de la regression

pacf(diff(FEM_AGT_QUA_TS))

model3FEM_AGT_QUA_TS <- ur.df(FEM_AGT_QUA_TS, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3FEM_AGT_QUA_TS) #### Affichage r?sultats du modele

model2FEM_AGT_QUA_TS <- ur.df(FEM_AGT_QUA_TS, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2FEM_AGT_QUA_TS) #### Affichage r?sultats du modele

model1FEM_AGT_QUA_TS <- ur.df(FEM_AGT_QUA_TS, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1FEM_AGT_QUA_TS) #### Affichage r?sultats du modele

pp.test(FEM_AGT_QUA)

pp.test(FEM_AGT_QUA_TS)

kpss.test(FEM_AGT_QUA_TS)

kpss.test(FEM_AGT_QUA)


#### 16-Test d'ADF sur Densité totale du Personnel de Santé Qualifiés (DENS_TOT)

DENS_TOT <- ts(DENS_TOT,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dDENS_TOT <- diff(DENS_TOT)#### Cr?ation de la variable diff?rienci?

ddDENS_TOT <- diff(dDENS_TOT)#### Cr?ation de la variable deux fois diff?renci?

pacf(DENS_TOT)#### Corr?logramme partielle de la variable

pacf(dDENS_TOT)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddDENS_TOT)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(DENS_TOT,col="blue",main="DENS_TOT",xlab="ann?e",ylab="DENS_TOT")#### Graphe de la variable

plot.ts(dDENS_TOT,col="blue",main="dDENS_TOT",xlab="ann?e",ylab="dDENS_TOT")#### Graphe de la variable

plot.ts(ddDENS_TOT ,col="blue",main="ddDENS_TOT",xlab="ann?e",ylab="ddDENS_TOT")#### Graphe de la variable

LagTest_DENS_TOT <- dynlm(DENS_TOT ~ L(DENS_TOT)+L(DENS_TOT,2)+L(DENS_TOT,3)+L(DENS_TOT,4))

LagTest_dDENS_TOT <- dynlm(dDENS_TOT ~ L(dDENS_TOT)+L(dDENS_TOT,2)+L(dDENS_TOT,3)+L(dDENS_TOT,4))

kable(tidy(LagTest_DENS_TOT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dDENS_TOT),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3DENS_TOT <- ur.df(DENS_TOT, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3DENS_TOT) #### Affichage r?sultats du modele

trend7 <- as.numeric(time(DENS_TOT))#### Extraction de la tendance de la s?rie

DENS_TOTts <- lm(DENS_TOT~trend7)#### Regression de la s?rie sur sa tendance 

summary.lm(DENS_TOTts)#### affichage r?sultat de la regression

DENS_TOT_TS <- DENS_TOTts$residuals#### extraction du r?sidus de la regression

pacf(diff(DENS_TOT_TS))

model3DENS_TOT_TS <- ur.df(DENS_TOT_TS, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3DENS_TOT_TS) #### Affichage r?sultats du modele

model2DENS_TOT_TS <- ur.df(DENS_TOT_TS, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2DENS_TOT_TS) #### Affichage r?sultats du modele

model1DENS_TOT_TS <- ur.df(DENS_TOT_TS, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1DENS_TOT_TS) #### Affichage r?sultats du modele

pp.test(DENS_TOT_TS)

pp.test(DENS_TOT)

kpss.test(DENS_TOT_TS)

kpss.test(DENS_TOT)


#### 17-Test d'ADF sur le Taux d’achèvement de l’école primaire (TB_ACHV_PRIM)

TB_ACHV_PRIM <- ts(TB_ACHV_PRIM,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTB_ACHV_PRIM <- diff(TB_ACHV_PRIM)#### Cr?ation de la variable diff?rienci?

ddTB_ACHV_PRIM <- diff(dTB_ACHV_PRIM)#### Cr?ation de la variable deux fois diff?renci?

pacf(TB_ACHV_PRIM)#### Corr?logramme partielle de la variable

pacf(dTB_ACHV_PRIM)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTB_ACHV_PRIM)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TB_ACHV_PRIM,col="blue",main="TB_ACHV_PRIM",xlab="ann?e",ylab="TB_ACHV_PRIM")#### Graphe de la variable

plot.ts(dTB_ACHV_PRIM,col="blue",main="dTB_ACHV_PRIM",xlab="ann?e",ylab="dTB_ACHV_PRIM")#### Graphe de la variable

plot.ts(ddTB_ACHV_PRIM ,col="blue",main="ddTB_ACHV_PRIM",xlab="ann?e",ylab="ddTB_ACHV_PRIM")#### Graphe de la variable

LagTest_TB_ACHV_PRIM <- dynlm(TB_ACHV_PRIM ~ L(TB_ACHV_PRIM)+L(TB_ACHV_PRIM,2)+L(TB_ACHV_PRIM,3)+L(TB_ACHV_PRIM,4))

LagTest_dTB_ACHV_PRIM <- dynlm(dTB_ACHV_PRIM ~ L(dTB_ACHV_PRIM)+L(dTB_ACHV_PRIM,2)+L(dTB_ACHV_PRIM,3)+L(dTB_ACHV_PRIM,4))

kable(tidy(LagTest_TB_ACHV_PRIM),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTB_ACHV_PRIM),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TB_ACHV_PRIM <- ur.df(TB_ACHV_PRIM, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TB_ACHV_PRIM) #### Affichage r?sultats du modele

trend8 <- as.numeric(time(TB_ACHV_PRIM))#### Extraction de la tendance de la s?rie

TB_ACHV_PRIMts <- lm(TB_ACHV_PRIM~trend8)#### Regression de la s?rie sur sa tendance 

summary.lm(TB_ACHV_PRIMts)#### affichage r?sultat de la regression

TB_ACHV_PRIM_TS <- TB_ACHV_PRIMts$residuals#### extraction du r?sidus de la regression

pacf(diff(TB_ACHV_PRIM_TS))

model3TB_ACHV_PRIM_TS <- ur.df(TB_ACHV_PRIM_TS, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TB_ACHV_PRIM_TS) #### Affichage r?sultats du modele

model2TB_ACHV_PRIM_TS <- ur.df(TB_ACHV_PRIM_TS, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TB_ACHV_PRIM_TS) #### Affichage r?sultats du modele

model1TB_ACHV_PRIM_TS <- ur.df(TB_ACHV_PRIM_TS, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TB_ACHV_PRIM_TS) #### Affichage r?sultats du modele

pp.test(TB_ACHV_PRIM_TS)

pp.test(TB_ACHV_PRIM)

kpss.test(TB_ACHV_PRIM_TS)

kpss.test(TB_ACHV_PRIM)


#### 18-Test d'ADF sur le Taux d’achèvement des filles dans  les écoles primaires (TB_ACHV_FIPRM)

TB_ACHV_FIPRM <- ts(TB_ACHV_FIPRM,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTB_ACHV_FIPRM <- diff(TB_ACHV_FIPRM)#### Cr?ation de la variable diff?rienci?

ddTB_ACHV_FIPRM <- diff(dTB_ACHV_FIPRM)#### Cr?ation de la variable deux fois diff?renci?

pacf(TB_ACHV_FIPRM)#### Corr?logramme partielle de la variable

pacf(dTB_ACHV_FIPRM)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTB_ACHV_FIPRM)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TB_ACHV_FIPRM,col="blue",main="TB_ACHV_FIPRM",xlab="ann?e",ylab="TB_ACHV_FIPRM")#### Graphe de la variable

plot.ts(dTB_ACHV_FIPRM,col="blue",main="dTB_ACHV_FIPRM",xlab="ann?e",ylab="dTB_ACHV_FIPRM")#### Graphe de la variable

plot.ts(ddTB_ACHV_FIPRM ,col="blue",main="ddTB_ACHV_FIPRM",xlab="ann?e",ylab="ddTB_ACHV_FIPRM")#### Graphe de la variable

LagTest_TB_ACHV_FIPRM <- dynlm(TB_ACHV_FIPRM ~ L(TB_ACHV_FIPRM)+L(TB_ACHV_FIPRM,2)+L(TB_ACHV_FIPRM,3)+L(TB_ACHV_FIPRM,4))

LagTest_dTB_ACHV_FIPRM <- dynlm(dTB_ACHV_FIPRM ~ L(dTB_ACHV_FIPRM)+L(dTB_ACHV_FIPRM,2)+L(dTB_ACHV_FIPRM,3)+L(dTB_ACHV_FIPRM,4))

kable(tidy(LagTest_TB_ACHV_FIPRM),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTB_ACHV_FIPRM),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TB_ACHV_FIPRM <- ur.df(TB_ACHV_FIPRM, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TB_ACHV_FIPRM) #### Affichage r?sultats du modele

trend9 <- as.numeric(time(TB_ACHV_FIPRM))#### Extraction de la tendance de la s?rie

TB_ACHV_FIPRMts <- lm(TB_ACHV_FIPRM~trend9)#### Regression de la s?rie sur sa tendance 

summary.lm(TB_ACHV_FIPRMts)#### affichage r?sultat de la regression

TB_ACHV_FIPRM_TS <- TB_ACHV_FIPRMts$residuals#### extraction du r?sidus de la regression

pacf(diff(TB_ACHV_FIPRM_TS))

model3TB_ACHV_FIPRM_TS <- ur.df(TB_ACHV_FIPRM_TS, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TB_ACHV_FIPRM_TS) #### Affichage r?sultats du modele

model2TB_ACHV_FIPRM_TS <- ur.df(TB_ACHV_FIPRM_TS, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TB_ACHV_FIPRM_TS) #### Affichage r?sultats du modele

model1TB_ACHV_FIPRM_TS <- ur.df(TB_ACHV_FIPRM_TS, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TB_ACHV_FIPRM_TS) #### Affichage r?sultats du modele

pp.test(TB_ACHV_FIPRM_TS)

pp.test(TB_ACHV_FIPRM)

kpss.test(TB_ACHV_FIPRM_TS)

kpss.test(TB_ACHV_FIPRM)


#### 19-Test d'ADF sur les Dépenses en éducation (DEP_EDUC)

DEP_EDUC <- ts(DEP_EDUC,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dDEP_EDUC <- diff(DEP_EDUC)#### Cr?ation de la variable diff?rienci?

ddDEP_EDUC <- diff(dDEP_EDUC)#### Cr?ation de la variable deux fois diff?renci?

pacf(DEP_EDUC)#### Corr?logramme partielle de la variable

pacf(dDEP_EDUC)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddDEP_EDUC)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(DEP_EDUC,col="blue",main="DEP_EDUC",xlab="ann?e",ylab="DEP_EDUC")#### Graphe de la variable

plot.ts(dDEP_EDUC,col="blue",main="dDEP_EDUC",xlab="ann?e",ylab="dDEP_EDUC")#### Graphe de la variable

plot.ts(ddDEP_EDUC ,col="blue",main="ddDEP_EDUC",xlab="ann?e",ylab="ddDEP_EDUC")#### Graphe de la variable

LagTest_DEP_EDUC<- dynlm(DEP_EDUC ~ L(DEP_EDUC)+L(DEP_EDUC,2)+L(DEP_EDUC,3)+L(DEP_EDUC,4))

LagTest_dDEP_EDUC <- dynlm(dDEP_EDUC ~ L(dDEP_EDUC)+L(dDEP_EDUC,2)+L(dDEP_EDUC,3)+L(dDEP_EDUC,4))

kable(tidy(LagTest_DEP_EDUC),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dDEP_EDUC),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3DEP_EDUC <- ur.df(DEP_EDUC, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3DEP_EDUC) #### Affichage r?sultats du modele

trend10 <- as.numeric(time(DEP_EDUC))#### Extraction de la tendance de la s?rie

DEP_EDUCts <- lm(DEP_EDUC~trend10)#### Regression de la s?rie sur sa tendance 

summary.lm(DEP_EDUCts)#### affichage r?sultat de la regression

DEP_EDUC_TS <- DEP_EDUCts$residuals#### extraction du r?sidus de la regression

pacf(diff(DEP_EDUC_TS))

model3DEP_EDUC_TS <- ur.df(DEP_EDUC_TS, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3DEP_EDUC_TS) #### Affichage r?sultats du modele

model2DEP_EDUC_TS <- ur.df(DEP_EDUC_TS, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2DEP_EDUC_TS) #### Affichage r?sultats du modele

model1DEP_EDUC_TS <- ur.df(DEP_EDUC_TS, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1DEP_EDUC_TS) #### Affichage r?sultats du modele

pp.test(DEP_EDUC_TS)

kpss.test(DEP_EDUC_TS)

pp.test(DEP_EDUC)

kpss.test(DEP_EDUC)


######## 20-Test d'ADF sur Taux d'urbanisation (TAU_URB)

TAU_URB <- ts(TAU_URB,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTAU_URB <- diff(TAU_URB)#### Cr?ation de la variable diff?rienci?

ddTAU_URB <- diff(dTAU_URB)#### Cr?ation de la variable deux fois diff?renci?

pacf(TAU_URB)#### Corr?logramme partielle de la variable

pacf(dTAU_URB)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTAU_URB)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TAU_URB,col="blue",main="TAU_URB",xlab="ann?e",ylab="TAU_URB")#### Graphe de la variable

plot.ts(dTAU_URB,col="blue",main="dTAU_URB",xlab="ann?e",ylab="dTAU_URB")#### Graphe de la variable

plot.ts(ddTAU_URB ,col="blue",main="ddTAU_URB",xlab="ann?e",ylab="ddTAU_URB")#### Graphe de la variable

LagTest_TAU_URB<- dynlm(TAU_URB ~ L(TAU_URB)+L(TAU_URB,2)+L(TAU_URB,3)+L(TAU_URB,4))

LagTest_dTAU_URB <- dynlm(dTAU_URB ~ L(dTAU_URB)+L(dTAU_URB,2)+L(dTAU_URB,3)+L(dTAU_URB,4))

kable(tidy(LagTest_TAU_URB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTAU_URB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TAU_URB <- ur.df(TAU_URB, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TAU_URB) #### Affichage r?sultats du modele

trend11 <- as.numeric(time(TAU_URB))#### Extraction de la tendance de la s?rie

TAU_URBts <- lm(TAU_URB~trend11)#### Regression de la s?rie sur sa tendance 

summary.lm(TAU_URBts)#### affichage r?sultat de la regression

TAU_URB_TS <- TAU_URBts$residuals#### extraction du r?sidus de la regression

pacf(diff(TAU_URB_TS))

model3TAU_URB_TS <- ur.df(TAU_URB_TS, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TAU_URB_TS) #### Affichage r?sultats du modele

model2TAU_URB_TS <- ur.df(TAU_URB_TS, type = "drift", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TAU_URB_TS) #### Affichage r?sultats du modele

model1TAU_URB_TS <- ur.df(TAU_URB_TS, type = "none", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TAU_URB_TS) #### Affichage r?sultats du modele

pp.test(TAU_URB_TS)

kpss.test(TAU_URB_TS)

pp.test(TAU_URB)

kpss.test(TAU_URB)


######## 21-Test d'ADF sur la Formation Brute de Capital Fixe (FBCF)

FBCF_CON <- ts(FBCF_CON,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dFBCF_CON <- diff(FBCF_CON)#### Cr?ation de la variable diff?rienci?

ddFBCF_CON <- diff(dFBCF_CON)#### Cr?ation de la variable deux fois diff?renci?

pacf(FBCF_CON)#### Corr?logramme partielle de la variable

pacf(dFBCF_CON)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddFBCF_CON)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(FBCF_CON,col="blue",main="FBCF_CON",xlab="ann?e",ylab="FBCF_CON")#### Graphe de la variable

plot.ts(dFBCF_CON,col="blue",main="dFBCF_CON",xlab="ann?e",ylab="dFBCF_CON")#### Graphe de la variable

plot.ts(ddFBCF_CON ,col="blue",main="ddFBCF_CON",xlab="ann?e",ylab="ddFBCF_CON")#### Graphe de la variable

LagTest_FBCF_CON <- dynlm(FBCF_CON ~ L(FBCF_CON)+L(FBCF_CON,2)+L(FBCF_CON,3)+L(FBCF_CON,4))

LagTest_dFBCF_CON <- dynlm(dFBCF_CON ~ L(dFBCF_CON)+L(dFBCF_CON,2)+L(dFBCF_CON,3)+L(dFBCF_CON,4))

kable(tidy(LagTest_FBCF_CON),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dFBCF_CON),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3FBCF_CON <- ur.df(FBCF_CON, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3FBCF_CON) #### Affichage r?sultats du modele

model2FBCF_CON <- ur.df(FBCF_CON, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2FBCF_CON) #### Affichage r?sultats du modele

model1FBCF_CON <- ur.df(FBCF_CON, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1FBCF_CON) #### Affichage r?sultats du modele

model3dFBCF_CON <- ur.df(dFBCF_CON, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dFBCF_CON) #### Affichage r?sultats du modele

model2dFBCF_CON <- ur.df(dFBCF_CON, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dFBCF_CON) #### Affichage r?sultats du modele

model1dFBCF_CON <- ur.df(dFBCF_CON, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1dFBCF_CON) #### Affichage r?sultats du modele

pp.test(FBCF_CON)

pp.test(dFBCF_CON)

kpss.test(FBCF_CON)

kpss.test(dFBCF_CON)


######## 22-Test d'ADF sur la Population Active POP_15_64

POP_15_64 <- ts(POP_15_64,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dPOP_15_64 <- diff(POP_15_64)#### Cr?ation de la variable diff?rienci?

ddPOP_15_64 <- diff(dPOP_15_64)#### Cr?ation de la variable deux fois diff?renci?

pacf(POP_15_64)#### Corr?logramme partielle de la variable

pacf(dPOP_15_64)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddPOP_15_64)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(POP_15_64,col="blue",main="POP_15_64",xlab="ann?e",ylab="POP_15_64")#### Graphe de la variable

plot.ts(dPOP_15_64,col="blue",main="dPOP_15_64",xlab="ann?e",ylab="dPOP_15_64")#### Graphe de la variable

plot.ts(ddPOP_15_64,col="blue",main="ddPOP_15_64",xlab="ann?e",ylab="ddPOP_15_64")#### Graphe de la variable

LagTest_POP_15_64 <- dynlm(POP_15_64 ~ L(POP_15_64)+L(POP_15_64,2)+L(POP_15_64,3)+L(POP_15_64,4))

LagTest_dPOP_15_64 <- dynlm(dPOP_15_64 ~ L(dPOP_15_64)+L(dPOP_15_64,2)+L(dPOP_15_64,3)+L(dPOP_15_64,4))

kable(tidy(LagTest_POP_15_64),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dPOP_15_64),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3POP_15_64 <- ur.df(POP_15_64, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3POP_15_64) #### Affichage r?sultats du modele$

trend12 <- as.numeric(time(POP_15_64))

POP_15_64ts <- lm(POP_15_64~trend12)#### Regression de la s?rie sur sa tendance 

summary.lm(POP_15_64ts)#### affichage r?sultat de la regression

POP_15_64_TS <- POP_15_64ts$residuals#### extraction du r?sidus de la regression

pacf(diff(POP_15_64_TS))

model3POP_15_64_TS <- ur.df(POP_15_64_TS, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3POP_15_64_TS) #### Affichage r?sultats du modele$

model2POP_15_64_TS <- ur.df(POP_15_64_TS, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2POP_15_64_TS) #### Affichage r?sultats du modele

model1POP_15_64_TS <- ur.df(POP_15_64_TS, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1POP_15_64_TS) #### Affichage r?sultats du modele

pp.test(POP_15_64_TS)

pp.test(POP_15_64)

kpss.test(POP_15_64_TS)

kpss.test(POP_15_64)


######## 23-Test d'ADF sur la Femme Active FEM_ACTV

FEM_ACTV <- ts(FEM_ACTV,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dFEM_ACTV <- diff(FEM_ACTV)#### Cr?ation de la variable diff?rienci?

ddFEM_ACTV <- diff(dFEM_ACTV)#### Cr?ation de la variable deux fois diff?renci?

pacf(FEM_ACTV)#### Corr?logramme partielle de la variable

pacf(dFEM_ACTV)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddFEM_ACTV)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(FEM_ACTV,col="blue",main="FEM_ACTV",xlab="ann?e",ylab="FEM_ACTV")#### Graphe de la variable

plot.ts(dFEM_ACTV,col="blue",main="dFEM_ACTV",xlab="ann?e",ylab="dFEM_ACTV")#### Graphe de la variable

plot.ts(ddFEM_ACTV,col="blue",main="ddFEM_ACTV",xlab="ann?e",ylab="ddFEM_ACTV")#### Graphe de la variable

LagTest_FEM_ACTV <- dynlm(FEM_ACTV ~ L(FEM_ACTV)+L(FEM_ACTV,2)+L(FEM_ACTV,3)+L(FEM_ACTV,4))

LagTest_dFEM_ACTV <- dynlm(dFEM_ACTV ~ L(dFEM_ACTV)+L(dFEM_ACTV,2)+L(dFEM_ACTV,3)+L(dFEM_ACTV,4))

kable(tidy(LagTest_FEM_ACTV),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dFEM_ACTV),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3FEM_ACTV <- ur.df(FEM_ACTV, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3FEM_ACTV) #### Affichage r?sultats du modele$

trend13 <- as.numeric(time(FEM_ACTV))

FEM_ACTVts <- lm(FEM_ACTV~trend13)#### Regression de la s?rie sur sa tendance 

summary.lm(FEM_ACTVts)#### affichage r?sultat de la regression

FEM_ACTV_TS <- FEM_ACTVts$residuals#### extraction du r?sidus de la regression

pacf(diff(FEM_ACTV_TS))

model3FEM_ACTV_TS <- ur.df(FEM_ACTV_TS, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3FEM_ACTV_TS) #### Affichage r?sultats du modele$

model2FEM_ACTV_TS <- ur.df(FEM_ACTV_TS, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2FEM_ACTV_TS) #### Affichage r?sultats du modele

model1FEM_ACTV_TS <- ur.df(FEM_ACTV_TS, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1FEM_ACTV_TS) #### Affichage r?sultats du modele

pp.test(FEM_ACTV_TS)

kpss.test(FEM_ACTV_TS)


######## 24-Test d'ADF sur le Taux de Fertilité des adolescentes (TAU_FER_ADO)

TAU_FER_ADO <- ts(TAU_FER_ADO,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTAU_FER_ADO <- diff(TAU_FER_ADO)#### Cr?ation de la variable diff?rienci?

ddTAU_FER_ADO <- diff(dTAU_FER_ADO)#### Cr?ation de la variable deux fois diff?renci?

pacf(TAU_FER_ADO)#### Corr?logramme partielle de la variable

pacf(dTAU_FER_ADO)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTAU_FER_ADO)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TAU_FER_ADO,col="blue",main="TAU_FER_ADO",xlab="ann?e",ylab="TAU_FER_ADO")#### Graphe de la variable

plot.ts(dTAU_FER_ADO,col="blue",main="dTAU_FER_ADO",xlab="ann?e",ylab="dTAU_FER_ADO")#### Graphe de la variable

plot.ts(ddTAU_FER_ADO,col="blue",main="ddTAU_FER_ADO",xlab="ann?e",ylab="ddTAU_FER_ADO")#### Graphe de la variable

LagTest_TAU_FER_ADO <- dynlm(TAU_FER_ADO ~ L(TAU_FER_ADO)+L(TAU_FER_ADO,2)+L(TAU_FER_ADO,3)+L(TAU_FER_ADO,4))

LagTest_dTAU_FER_ADO <- dynlm(dTAU_FER_ADO ~ L(dTAU_FER_ADO)+L(dTAU_FER_ADO,2)+L(dTAU_FER_ADO,3)+L(dTAU_FER_ADO,4))

kable(tidy(LagTest_TAU_FER_ADO),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTAU_FER_ADO),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TAU_FER_ADO <- ur.df(TAU_FER_ADO, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TAU_FER_ADO) #### Affichage r?sultats du modele

trend14 <- as.numeric(time(TAU_FER_ADO))

TAU_FER_ADOts <- lm(TAU_FER_ADO~trend14)#### Regression de la s?rie sur sa tendance 

summary.lm(TAU_FER_ADOts)#### affichage r?sultat de la regression

TAU_FER_ADO_TS <- TAU_FER_ADOts$residuals#### extraction du r?sidus de la regression

pacf(diff(TAU_FER_ADO_TS))

model3TAU_FER_ADO_TS <- ur.df(TAU_FER_ADO_TS, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TAU_FER_ADO_TS) #### Affichage r?sultats du modele$

model2TAU_FER_ADO_TS<- ur.df(TAU_FER_ADO_TS, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TAU_FER_ADO_TS) #### Affichage r?sultats du modele

model1TAU_FER_ADO_TS <- ur.df(TAU_FER_ADO_TS, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TAU_FER_ADO_TS) #### Affichage r?sultats du modele

pp.test(TAU_FER_ADO_TS)

kpss.test(TAU_FER_ADO_TS)


######## 25 -Test d'ADF sur le Taux Overture (TAU_OUV)

TAU_OUV <- ts(TAU_OUV,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dTAU_OUV <- diff(TAU_OUV)#### Cr?ation de la variable diff?rienci?

ddTAU_OUV <- diff(dTAU_OUV)#### Cr?ation de la variable deux fois diff?renci?

pacf(TAU_OUV)#### Corr?logramme partielle de la variable

pacf(dTAU_OUV)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddTAU_OUV)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(TAU_OUV,col="blue",main="TAU_OUV",xlab="ann?e",ylab="TAU_OUV")#### Graphe de la variable

plot.ts(dTAU_OUV,col="blue",main="dTAU_OUV",xlab="ann?e",ylab="dTAU_OUV")#### Graphe de la variable

plot.ts(ddTAU_OUV,col="blue",main="ddTAU_OUV",xlab="ann?e",ylab="ddTAU_OUV")#### Graphe de la variable

LagTest_TAU_OUV <- dynlm(TAU_OUV ~ L(TAU_OUV)+L(TAU_OUV,2)+L(TAU_OUV,3)+L(TAU_OUV,4))

LagTest_dTAU_OUV <- dynlm(dTAU_OUV ~ L(dTAU_OUV)+L(dTAU_OUV,2)+L(dTAU_OUV,3)+L(dTAU_OUV,4))

kable(tidy(LagTest_TAU_OUV),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dTAU_OUV),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3TAU_OUV <- ur.df(TAU_OUV, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TAU_OUV) #### Affichage r?sultats du modele

trend15 <- as.numeric(time(TAU_OUV))

TAU_OUVts <- lm(TAU_OUV~trend15)#### Regression de la s?rie sur sa tendance 

summary.lm(TAU_OUVts)#### affichage r?sultat de la regression

TAU_OUV_TS <- TAU_OUVts$residuals#### extraction du r?sidus de la regression

pacf(diff(TAU_OUV_TS))

model3TAU_OUV_TS <- ur.df(TAU_OUV_TS, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3TAU_OUV_TS) #### Affichage r?sultats du modele$

model2TAU_OUV_TS<- ur.df(TAU_OUV_TS, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2TAU_OUV_TS) #### Affichage r?sultats du modele

model1TAU_OUV_TS <- ur.df(TAU_OUV_TS, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1TAU_OUV_TS) #### Affichage r?sultats du modele

pp.test(TAU_OUV_TS)

pp.test(TAU_OUV)

kpss.test(TAU_OUV_TS)

kpss.test(TAU_OUV_TS)

######## 26-Test d'ADF sur la Formation Brute de Capital Fixe (FBCF_HAB)

FBCF_HAB <- ts(FBCF_HAB,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dFBCF_HAB <- diff(FBCF_HAB)#### Cr?ation de la variable diff?rienci?

ddFBCF_HAB <- diff(dFBCF_HAB)#### Cr?ation de la variable deux fois diff?renci?

pacf(FBCF_HAB)#### Corr?logramme partielle de la variable

pacf(dFBCF_HAB)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddFBCF_HAB)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(FBCF_HAB,col="blue",main="FBCF_HAB",xlab="ann?e",ylab="FBCF_HAB")#### Graphe de la variable

plot.ts(dFBCF_HAB,col="blue",main="dFBCF_HAB",xlab="ann?e",ylab="dFBCF_HAB")#### Graphe de la variable

plot.ts(ddFBCF_HAB ,col="blue",main="ddFBCF_HAB",xlab="ann?e",ylab="ddFBCF_HAB")#### Graphe de la variable

LagTest_FBCF_HAB <- dynlm(FBCF_HAB ~ L(FBCF_HAB)+L(FBCF_HAB,2)+L(FBCF_HAB,3)+L(FBCF_HAB,4))

LagTest_dFBCF_HAB <- dynlm(dFBCF_HAB ~ L(dFBCF_HAB)+L(dFBCF_HAB,2)+L(dFBCF_HAB,3)+L(dFBCF_HAB,4))

kable(tidy(LagTest_FBCF_HAB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dFBCF_HAB),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3FBCF_HAB <- ur.df(FBCF_HAB, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3FBCF_HAB) #### Affichage r?sultats du modele

model2FBCF_HAB <- ur.df(FBCF_HAB, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2FBCF_HAB) #### Affichage r?sultats du modele

model1FBCF_HAB <- ur.df(FBCF_HAB, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1FBCF_HAB) #### Affichage r?sultats du modele

model3dFBCF_HAB <- ur.df(dFBCF_HAB, type = "trend", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dFBCF_HAB) #### Affichage r?sultats du modele

model2dFBCF_HAB <- ur.df(dFBCF_HAB, type = "drift", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2dFBCF_HAB) #### Affichage r?sultats du modele

model1dFBCF_HAB <- ur.df(dFBCF_HAB, type = "none", lags = 1)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1dFBCF_HAB) #### Affichage r?sultats du modele

pp.test(FBCF_HAB)

pp.test(dFBCF_HAB)

kpss.test(FBCF_HAB)

kpss.test(dFBCF_HAB)


######## 26-Test d'ADF sur la proportion de Femme agents de santé (PRO_FEM_AGT_QUA)

PRO_FEM_AGT_QUA <- ts(PRO_FEM_AGT_QUA,start = 1985, end = 2021, frequency = 1)#### transformation de la variable en s?rie temporelle

dPRO_FEM_AGT_QUA <- diff(PRO_FEM_AGT_QUA)#### Cr?ation de la variable diff?rienci?

ddPRO_FEM_AGT_QUA <- diff(dPRO_FEM_AGT_QUA)#### Cr?ation de la variable deux fois diff?renci?

pacf(PRO_FEM_AGT_QUA)#### Corr?logramme partielle de la variable

pacf(dPRO_FEM_AGT_QUA)#### Corr?logramme partielle de la variable diff?rienci? pour le choix du lag ou retard optimal de la variable

pacf(ddPRO_FEM_AGT_QUA)#### Corr?logramme partielle de la variable deux fois diff?renci? pour le choix du lag ou retard optimal de la variable différentielle

plot.ts(PRO_FEM_AGT_QUA,col="blue",main="PRO_FEM_AGT_QUA",xlab="ann?e",ylab="PRO_FEM_AGT_QUA")#### Graphe de la variable

plot.ts(dPRO_FEM_AGT_QUA,col="blue",main="dPRO_FEM_AGT_QUA",xlab="ann?e",ylab="dPRO_FEM_AGT_QUA")#### Graphe de la variable

plot.ts(ddPRO_FEM_AGT_QUA ,col="blue",main="ddPRO_FEM_AGT_QUA",xlab="ann?e",ylab="ddPRO_FEM_AGT_QUAB")#### Graphe de la variable

LagTest_PRO_FEM_AGT_QUA <- dynlm(PRO_FEM_AGT_QUA ~ L(PRO_FEM_AGT_QUA)+L(PRO_FEM_AGT_QUA,2)+L(PRO_FEM_AGT_QUA,3)+L(PRO_FEM_AGT_QUA,4))

LagTest_dPRO_FEM_AGT_QUA <- dynlm(dPRO_FEM_AGT_QUA ~ L(dPRO_FEM_AGT_QUA)+L(dPRO_FEM_AGT_QUA,2)+L(dPRO_FEM_AGT_QUA,3)+L(dPRO_FEM_AGT_QUA,4))

kable(tidy(LagTest_PRO_FEM_AGT_QUA),  digits=4,caption="Autoregressive model of order 4 using the dataset")

kable(tidy(LagTest_dPRO_FEM_AGT_QUA),  digits=4,caption="Autoregressive model of order 4 using the dataset")

model3PRO_FEM_AGT_QUA <- ur.df(PRO_FEM_AGT_QUA, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3PRO_FEM_AGT_QUA) #### Affichage r?sultats du modele

trend16 <- as.numeric(time(PRO_FEM_AGT_QUA))

PRO_FEM_AGT_QUAts <- lm(PRO_FEM_AGT_QUA~trend16)#### Regression de la s?rie sur sa tendance 

summary.lm(PRO_FEM_AGT_QUAts)#### affichage r?sultat de la regression

PRO_FEM_AGT_QUA_TS <- PRO_FEM_AGT_QUAts$residuals#### extraction du r?sidus de la regression

pacf(diff(PRO_FEM_AGT_QUA_TS))

model3PRO_FEM_AGT_QUA_TS <- ur.df(PRO_FEM_AGT_QUA_TS, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3PRO_FEM_AGT_QUA_TS) #### Affichage r?sultats du modele$

model2PRO_FEM_AGT_QUA_TS<- ur.df(PRO_FEM_AGT_QUA_TS, type = "drift", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model2PRO_FEM_AGT_QUA_TS) #### Affichage r?sultats du modele

model1PRO_FEM_AGT_QUA_TS <- ur.df(PRO_FEM_AGT_QUA_TS, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model1PRO_FEM_AGT_QUA_TS) #### Affichage r?sultats du modele

adf.test(PRO_FEM_AGT_QUA)

pp.test(PRO_FEM_AGT_QUA_TS)

kpss.test(PRO_FEM_AGT_QUA_TS)


####################################### Test de Stationnarités Finales des variables retenues 

adf.test(ESP_VIE)

pp.test(dESP_VIE, type ="Z_rho")

kpss.test(ESP_VIE)


adf.test(TAU_MOR_INF)

pp.test(TAU_MOR_INF,type ="Z_tau")

kpss.test(TAU_MOR_INF)

adf.test(MORT_MAT)

pp.test(dMORT_MAT,type ="Z_tau")

kpss.test(MORT_MAT)

adf.test(IND_FEC)

pp.test(IND_FEC,type ="Z_tau")

kpss.test(IND_FEC)

adf.test(DENS_TOT_TS)

pp.test(DENS_TOT_TS,type ="Z_tau")

kpss.test(DENS_TOT_TS)

adf.test(PRO_FEM_AGT_QUA_TS)

pp.test(PRO_FEM_AGT_QUA_TS,type ="Z_tau")

kpss.test(PRO_FEM_AGT_QUA_TS)

adf.test(TB_ACHV_PRIM_TS)

pp.test(TB_ACHV_PRIM_TS, type ="Z_tau")

kpss.test(TB_ACHV_PRIM_TS)

adf.test(TB_ACHV_FIPRM_TS)

pp.test(TB_ACHV_FIPRM_TS,type ="Z_tau")

kpss.test(TB_ACHV_FIPRM_TS)

adf.test(INC_PAL_TS)

pp.test(INC_PAL_TS,type ="Z_tau")

kpss.test(INC_PAL_TS)

adf.test(INC_ANE)

pp.test(INC_ANE,type ="Z_rho")

kpss.test(INC_ANE)

adf.test(INC_IRA)

pp.test(INC_IRA,type ="Z_tau")

kpss.test(INC_IRA)

adf.test(dLN_PIB_HAB)

pp.test(dLN_PIB_HAB, type ="Z_tau")

kpss.test(LN_PIB_HAB)

adf.test(dLN_DEP_SANT_HAB)

pp.test(dLN_DEP_SANT_HAB, type ="Z_tau")

kpss.test(LN_DEP_SANT_HAB)




###################################### Correlations des variables

#################### Variables enlevées

Base_init <-data.frame(LN_PIB_HAB, LN_DEP_SANT_HAB,LN_BUDG_SANT_HAB, ESP_VIE, IND_FEC, MORT_MAT,TAU_MOR_INF, DENS_TOT, TB_ACHV_PRIM, POP_15_64, FBCF_HAB, TAU_URB)

ggpairs(Base_init)

cor(Base_init)

#################### Variables enlevées 2

Base_init2 <-data.frame(LN_PIB, LN_DEP_SANT, LN_BUDG_NAT, LN_BUDG_PERS, LN_BUDG_SANT, ESP_VIE, IND_FEC, MORT_MAT,TAU_MOR_INF, DENS_TOT, TB_ACHV_PRIM, POP_15_64, FBCF_CON, TAU_URB)

ggpairs(Base_init2)

cor(Base_init2)


############ Variables Modèle Densité Personnel

Base_Pers_San <-data.frame(ESP_VIE, IND_FEC, MORT_MAT,TAU_MOR_INF, INC_PAL, INC_ANE,INC_IRA, DENS_TOT, LN_BUDG_SANT, LN_BUDG_PERS, TB_ACHV_PRIM, TB_ACHV_FIPRM, PRO_FEM_AGT_QUA)

ggpairs(Base_Pers_San)

cor(Base_Pers_San)

############ Variables Modèle Santé et Croissance économique

Base_Ind_San <-data.frame(LN_PIB_HAB,LN_DEP_SANT_HAB,LN_BUDG_SANT_HAB, ESP_VIE, IND_FEC, TAU_MOR_INF, MORT_MAT, DENS_TOT, TB_ACHV_PRIM, PRO_FEM_AGT_QUA, TB_ACHV_FIPRM, INC_PAL)

ggpairs(Base_Ind_San)

cor(Base_Ind_San)

############ Variables Modèle Inégalité, Santé et Croissance économique

Base_Fem <-data.frame(LN_PIB_HAB, LN_DEP_SANT_HAB, ESP_VIE ,IND_FEC ,TAU_MOR_INF,MORT_MAT , PRO_FEM_AGT_QUA,TB_ACHV_FIPRM, DENS_TOT,INC_PAL)

ggpairs(Base_Fem)

cor(Base_Fem)




############################### CAUSALITE ENTRE LES VARIABLES

#Null hypothesis x don't cause y

################## MODELE PERSONNEL DE SANTE ET INDICATEURS DE SANTE

grangertest(DENS_TOT ~ ESP_VIE, order = 1)

grangertest(DENS_TOT ~ TAU_MOR_INF, order = 1)

grangertest(DENS_TOT ~ IND_FEC, order = 1)

grangertest(DENS_TOT ~ INC_PAL, order = 1)

grangertest(DENS_TOT ~ INC_IRA, order = 1)

grangertest(DENS_TOT ~ TB_ACHV_PRIM, order = 1)

grangertest(DENS_TOT ~ PRO_FEM_AGT_QUA, order = 1)

grangertest(DENS_TOT ~ LN_PIB_HAB, order = 1)

grangertest(DENS_TOT ~ TB_MORT, order = 1)

grangertest(ESP_VIE ~ TB_ACHV_PRIM, order = 3)

grangertest(ESP_VIE ~ TAU_MOR_INF, order = 5)

grangertest(ESP_VIE ~ MORT_MAT, order = 11) ####### Seuil 10%

grangertest(ESP_VIE ~ LN_PIB_HAB, order = 1)

grangertest(ESP_VIE ~ LN_DEP_SANT_HAB, order = 1)

grangertest(TAU_MOR_INF ~ ESP_VIE, order = 3)

grangertest(TAU_MOR_INF ~ INC_PAL, order = 2)

grangertest(TAU_MOR_INF ~ LN_PIB_HAB, order = 1)

grangertest(TAU_MOR_INF ~ LN_DEP_SANT_HAB, order = 1)

grangertest(IND_FEC ~ INC_PAL, order = 2)

grangertest(IND_FEC ~ INC_ANE, order = 9)

grangertest(IND_FEC ~ ESP_VIE, order = 2)

grangertest(IND_FEC ~ LN_PIB_HAB, order = 1)

grangertest(IND_FEC ~ LN_DEP_SANT_HAB, order = 2)

grangertest(IND_FEC ~ TB_MORT, order = 10)

grangertest(MORT_MAT ~ TAU_MOR_INF, order = 7)

grangertest(MORT_MAT ~ TB_ACHV_PRIM, order = 2)

grangertest(MORT_MAT ~ LN_PIB_HAB, order = 9)

grangertest(MORT_MAT ~ LN_DEP_SANT_HAB, order = 1)

grangertest(INC_PAL ~ DENS_TOT, order = 1)

grangertest(INC_PAL ~ TB_ACHV_PRIM, order = 2)

grangertest(INC_PAL ~ ESP_VIE, order = 2)

grangertest(INC_PAL ~ TAU_MOR_INF, order = 2)

grangertest(INC_PAL ~ IND_FEC, order = 2)

grangertest(INC_PAL ~ LN_PIB_HAB, order = 1)

grangertest(INC_PAL ~ TB_MORT, order = 2)

grangertest(INC_IRA ~ LN_PIB_HAB, order = 6)

grangertest(INC_IRA ~ DENS_TOT, order = 2) ####### Seuil 10%

grangertest(INC_IRA ~ ESP_VIE, order = 5)

grangertest(INC_IRA ~ TAU_MOR_INF, order = 9)

grangertest(INC_IRA ~ IND_FEC, order = 6)

grangertest(INC_ANE ~ ESP_VIE, order = 3)####### 10%

grangertest(INC_ANE ~ MORT_MAT, order = 3)

grangertest(INC_ANE ~ TB_ACHV_PRIM, order = 7)

grangertest(INC_ANE ~ LN_PIB_HAB, order = 3)

grangertest(INC_ANE ~ LN_DEP_SANT_HAB, order = 2)

grangertest(TB_ACHV_PRIM ~ DENS_TOT, order = 1)

grangertest(TB_ACHV_PRIM ~ TAU_MOR_INF, order = 1)

grangertest(TB_ACHV_PRIM ~ IND_FEC, order = 1)

grangertest(TB_ACHV_PRIM ~ ESP_VIE, order = 1)

grangertest(TB_ACHV_PRIM ~ MORT_MAT, order = 3)

grangertest(TB_ACHV_PRIM ~ TB_ACHV_FIPRM, order = 1)

grangertest(TB_ACHV_PRIM ~ LN_PIB_HAB, order = 1)

grangertest(TB_ACHV_PRIM ~ LN_DEP_SANT_HAB, order = 9)

grangertest(TB_MORT ~ TAU_MOR_INF, order = 2)

grangertest(TB_MORT ~ INC_ANE, order = 2)

grangertest(TB_MORT ~ INC_PAL, order = 2)

grangertest(TB_MORT ~ INC_IRA, order = 3)

grangertest(TB_MORT ~ DENS_TOT, order = 2)

grangertest(TB_MORT ~ IND_FEC, order = 1)

grangertest(TB_MORT ~ ESP_VIE, order = 1)

grangertest(TB_MORT ~ TAU_MOR_INF, order = 1)

grangertest(TB_MORT ~ LN_PIB_HAB, order = 1)

grangertest(TB_ACHV_FIPRM ~ DENS_TOT, order = 1)

grangertest(TB_ACHV_FIPRM ~ TAU_MOR_INF, order = 1)

grangertest(TB_ACHV_FIPRM ~ IND_FEC, order = 1)

grangertest(TB_ACHV_FIPRM ~ ESP_VIE, order = 1)

grangertest(TB_ACHV_FIPRM ~ MORT_MAT, order = 1)

grangertest(TB_ACHV_FIPRM ~ TB_ACHV_PRIM, order = 2)

grangertest(TB_ACHV_FIPRM ~ LN_PIB_HAB, order = 1)

grangertest(PRO_FEM_AGT_QUA ~ ESP_VIE, order = 1)

grangertest(PRO_FEM_AGT_QUA ~ TAU_MOR_INF, order = 1)

grangertest(PRO_FEM_AGT_QUA ~ IND_FEC, order = 1)

grangertest(PRO_FEM_AGT_QUA ~ INC_PAL, order = 1)

grangertest(PRO_FEM_AGT_QUA ~ TB_ACHV_FIPRM, order = 1)

grangertest(PRO_FEM_AGT_QUA ~ TB_ACHV_PRIM, order = 2)

grangertest(PRO_FEM_AGT_QUA ~ DENS_TOT, order = 1)

grangertest(PRO_FEM_AGT_QUA ~ LN_PIB_HAB, order = 1)

grangertest(LN_PIB_HAB ~ ESP_VIE, order = 1)

grangertest(LN_PIB_HAB ~ TAU_MOR_INF, order = 1)

grangertest(LN_PIB_HAB ~ IND_FEC, order = 1)

grangertest(LN_PIB_HAB ~ MORT_MAT, order = 1)

grangertest(LN_PIB_HAB ~ TB_MORT, order = 3)

grangertest(LN_PIB_HAB ~ LN_DEP_SANT_HAB, order = 8)

grangertest(LN_PIB_HAB ~ INC_IRA, order = 9)

grangertest(LN_PIB_HAB ~ PRO_FEM_AGT_QUA, order = 5)

grangertest(LN_DEP_SANT_HAB ~ DENS_TOT, order = 1)

grangertest(LN_DEP_SANT_HAB ~ TAU_MOR_INF, order = 1)

grangertest(LN_DEP_SANT_HAB ~ IND_FEC, order = 2)

grangertest(LN_DEP_SANT_HAB ~ INC_PAL, order = 9)

grangertest(LN_DEP_SANT_HAB ~ INC_ANE, order = 9)

grangertest(LN_DEP_SANT_HAB ~ TB_ACHV_PRIM, order = 2)

grangertest(LN_DEP_SANT_HAB ~ TB_ACHV_FIPRM, order = 2)

grangertest(LN_DEP_SANT_HAB ~ PRO_FEM_AGT_QUA, order = 1)

grangertest(LN_DEP_SANT_HAB ~ LN_PIB_HAB, order = 1)

grangertest(LN_DEP_SANT_HAB ~ TB_MORT, order = 7)

################################################################## ESTIMATIONS DES MODELES

###################################################### MODELE DENSITE DU PERSONNEL

############################ ESP_VIE

MCEltP.1 <- lm(ESP_VIE ~ DENS_TOT + FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + MORT_MAT +  IND_FEC + TAU_MOR_INF + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA + CRO_PIB + CRO_PIB_HAB + CRO_POP, data = Modèle1)

check_collinearity(MCEltP.1)

MCEltP.2 <- update(MCEltP.1, . ~ . - CRO_POP- TAU_MOR_INF - MORT_MAT - FEM_AGT_QUA - TAU_URB - IND_FEC - POP_15_64 - FEM_ACTV - TB_ACHV_FIPRM - DEP_EDUC - TB_MORT + Dummy85_89 + Dummy21 - CRO_PIB - CRO_PIB_HAB,data = Modèle1)

check_collinearity(MCEltP.2)

ggpairs(MCEltP.2)

summary(MCEltP.2)

par(mfrow=c(2,2))

plot(MCEltP.2)

outlierTest(MCEltP.2)

resP.2 <- MCEltP.2$residuals

jarque.bera.test(resP.2)

dwtest(MCEltP.2)

bgtest(MCEltP.2)

bptest(MCEltP.2)

ncvTest(MCEltP.2)

qcc::cusum(resP.2)

resettest(MCEltP.2)


############################ IND_FEC

MCEltP2.1 <- lm(IND_FEC ~ DENS_TOT + FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + MORT_MAT +  ESP_VIE + TAU_MOR_INF + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA)

check_collinearity(MCEltP2.1)

MCEltP2.2 <- update(MCEltP2.1, . ~ . - POP_15_64 - DEP_EDUC - MORT_MAT - TAU_MOR_INF - FEM_ACTV - TB_MORT - TB_ACHV_FIPRM - FEM_AGT_QUA - TAU_URB - ESP_VIE + Dummy85_90 + Dummy21)

check_collinearity(MCEltP2.2)

ggpairs(MCEltP2.2)

summary(MCEltP2.2)

par(mfrow=c(2,2))

plot(MCEltP2.2)

outlierTest(MCEltP2.2)

resP2.2 <- MCEltP2.2$residuals

jarque.bera.test(resP2.2)

dwtest(MCEltP2.2)

bgtest(MCEltP2.2)

bptest(MCEltP2.2)

ncvTest(MCEltP2.2)

qcc::cusum(resP2.2)

resettest(MCEltP2.2)


################## TAU_MOR_INF

MCEltP3.1 <- lm(TAU_MOR_INF ~ DENS_TOT + FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + MORT_MAT +  ESP_VIE + IND_FEC + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA)

check_collinearity(MCEltP3.1)

MCEltP3.2 <- update(MCEltP3.1, . ~ . - FEM_AGT_QUA - FEM_ACTV - POP_15_64 - IND_FEC - TB_ACHV_FIPRM - TAU_URB - ESP_VIE - TB_MORT - MORT_MAT - DEP_EDUC + Dummy85_89 + Dummy21)

check_collinearity(MCEltP3.2)

ggpairs(MCEltP3.2)

summary(MCEltP3.2)

par(mfrow=c(2,2))

plot(MCEltP3.2)

outlierTest(MCEltP3.2)

resP3.2 <- MCEltP3.2$residuals

jarque.bera.test(resP3.2)

dwtest(MCEltP3.2)

bgtest(MCEltP3.2)

bptest(MCEltP3.2)

ncvTest(MCEltP3.2)

qcc::cusum(resP3.2)

resettest(MCEltP3.2)

################## MORT_MAT

MCEltP4.1 <- lm(MORT_MAT ~ DENS_TOT + FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + TAU_MOR_INF +  ESP_VIE + IND_FEC + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA)

check_collinearity(MCEltP4.1)

MCEltP4.2 <- update(MCEltP4.1, . ~ . - FEM_AGT_QUA - FEM_ACTV - POP_15_64 - IND_FEC - TB_ACHV_FIPRM - TAU_URB - ESP_VIE - TB_MORT - TAU_MOR_INF - DEP_EDUC + Dummy85_90 + Dummy19_21)

check_collinearity(MCEltP4.2)

ggpairs(MCEltP4.2)

summary(MCEltP4.2)

outlierTest(MCEltP4.2)

par(mfrow=c(2,2))

plot(MCEltP4.2)

resP4.2 <- MCEltP4.2$residuals

jarque.bera.test(resP4.2)

bgtest(MCEltP4.2)

bptest(MCEltP4.2)

ncvTest(MCEltP4.2)

qcc::cusum(resP4.2)

resettest(MCEltP4.2)


###################################################################### CROISSANCE ECONOMIQUE ET SANTE


########################################### ARDL


############################# MODELE ESP_VIE

MCElt <- lm(LN_PIB_HAB ~ DENS_TOT + TAU_MOR_INF + LN_DEP_SANT_HAB + ESP_VIE + MORT_MAT + IND_FEC + POP_15_64 + FBCF_HAB + TB_ACHV_PRIM + TB_ACHV_FIPRM)

check_collinearity(MCElt)

summary(MCElt)

MCElt2 <- update(MCElt, . ~ . + LN_DEP_SANT_HAB - POP_15_64 - TAU_MOR_INF - IND_FEC - FBCF_HAB - MORT_MAT - TB_ACHV_PRIM - TB_ACHV_FIPRM + Dummy89 + Dummy94 + LN_BUDG_PERS)

check_collinearity(MCElt2)

summary(MCElt2)

plot(MCElt2)

outlierTest(MCElt2)

res <- MCElt2$residuals

jarque.bera.test(res)

bgtest(MCElt2)

bptest(MCElt2)

qcc::cusum(res)


############### ARDL LONG TERME ET COURT TERME ESP_VIE

model <- ARDL::auto_ardl(LN_PIB_HAB ~ DENS_TOT + ESP_VIE + LN_DEP_SANT_HAB + TB_ACHV_PRIM + Dummy89 + Dummy94, data = Modèle1, max_order = c(2,2,2,2,2,2,2), selection = "BIC")

model$top_orders

model <- ARDL::ardl(LN_PIB_HAB ~ DENS_TOT + ESP_VIE + LN_DEP_SANT_HAB + TB_ACHV_PRIM + Dummy89 + Dummy94, data = Modèle1, order = c(1,1,1,1,1,1,1))

summary(model)

check_collinearity(model)

outlierTest(model)

par(mfrow=c(2,2))

plot(model)

bgtest(model)

bptest(model)

jarque.bera.test(resid(model))

qcc::cusum(resid(model))

uecm <-uecm(model, case = 3)

check_collinearity(uecm)

summary(uecm)

resuecm <-resid(uecm)

cor(data.frame(uecm$model, resuecm))

bgtest(uecm)

bptest(uecm)

jarque.bera.test(resid(uecm))

qcc::cusum(resid(uecm))

recm <- recm(uecm, case = 3)

summary(recm)

multicollinearity(recm)

plot(recm)

bgtest(recm)

bptest(recm)

jarque.bera.test(resid(recm))

qcc::cusum(resid(recm))

######################## BOUND TEST ESP_VIE

modelecm <- ardlBound(data = Modèle1 , formula = LN_PIB_HAB ~ LN_DEP_SANT_HAB + ESP_VIE + DENS_TOT + TB_ACHV_PRIM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecm$ARDL.model)

summary(modelecm$ECM$EC.model)

modelecm$bg

modelecm$bp

modelecm$lb

modelecm$sp


#################################### MODELE TAU_MOR_INF

MCElt1.2 <- lm(LN_PIB_HAB ~ TAU_MOR_INF + LN_DEP_SANT_HAB + DENS_TOT + TB_ACHV_FIPRM + Dummy89 + Dummy94)

check_collinearity(MCElt1.2)

summary(MCElt1.2)

res1.2 <- MCElt1.2$residuals

jarque.bera.test(res1.2)

bgtest(MCElt1.2)

bptest(MCElt1.2)

qcc::cusum(res1.2)


############### ARDL LONG TERME ET COURT TERME TAU_MORT_INF

model2 <- ARDL::auto_ardl(LN_PIB_HAB ~ DENS_TOT + TAU_MOR_INF + LN_DEP_SANT_HAB + TB_ACHV_PRIM + Dummy89 + Dummy94, data = Modèle1, max_order = c(2,2,2,2,2,2,2), selection = "BIC")

model2$top_orders

model2 <- ARDL::ardl(LN_PIB_HAB ~ DENS_TOT + TAU_MOR_INF + LN_DEP_SANT_HAB + TB_ACHV_PRIM  + Dummy89 + Dummy94, data = Modèle1, order = c(1,1,1,1,1,1,1))

summary(model2)

check_collinearity(model2)

par(mfrow=c(2,2))

plot(model2)

res2.2 <- model2$residuals

jarque.bera.test(res2.2)

bgtest(model2)

bptest(model2)

qcc::cusum(res2.2)

uecm2 <-uecm(model2)

summary(uecm2)

check_collinearity(uecm2)

plot(uecm2)

bgtest(uecm2)

bptest(uecm2)

jarque.bera.test(resid(uecm2))

qcc::cusum(resid(uecm2))

recm2 <- recm(uecm2, case = 3)

summary(recm2)

multicollinearity(recm2)

par(mfrow=c(2,2))

plot(recm2)

bgtest(recm2)

bptest(recm2)

jarque.bera.test(resid(recm2))

qcc::cusum(resid(recm2))


######################## BOUND TEST TAU_MORT_INF

modelecm2 <- ardlBound(data = Modèle1 , formula = LN_PIB_HAB ~ LN_DEP_SANT_HAB + TAU_MOR_INF + DENS_TOT + TB_ACHV_PRIM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecm2$ARDL.model)

summary(modelecm2$ECM$EC.model)

modelecm2$bg

modelecm2$bp

modelecm2$lb

modelecm2$sp

#################################### MODELE IND_FEC

MCElt1.3 <- lm(LN_PIB_HAB ~ IND_FEC + LN_DEP_SANT_HAB + DENS_TOT + TB_ACHV_FIPRM + Dummy89 + Dummy94)

check_collinearity(MCElt1.3)

par(mfrow=c(2,2))

plot(MCElt1.3)

summary(MCElt1.3)

res1.3 <- MCElt1.3$residuals

jarque.bera.test(res1.3)

bgtest(MCElt1.3)

bptest(MCElt1.3)

qcc::cusum(res1.3)

############### ARDL LONG TERME ET COURT TERME IND_FEC

model3 <- ARDL::auto_ardl(LN_PIB_HAB ~ DENS_TOT + IND_FEC + LN_DEP_SANT_HAB + TB_ACHV_PRIM +Dummy89 + Dummy94, data = Modèle1, max_order = c(2,2,2,2,2,2,2), selection = "BIC")

model3$top_orders

model3 <- ARDL::ardl(LN_PIB_HAB ~ DENS_TOT + IND_FEC + LN_DEP_SANT_HAB + TB_ACHV_PRIM  + Dummy89 + Dummy94, data = Modèle1, order = c(1,1,1,1,1,1,1))

summary(model3)

res3.1 <- model3$residuals

jarque.bera.test(res3.1)

bgtest(model3)

bptest(model3)

qcc::cusum(res3.1)

uecm3 <-uecm(model3)

summary(uecm3)

check_collinearity(uecm3)

par(mfrow=c(2,2))

plot(uecm3)

bgtest(uecm3)

bptest(uecm3)

jarque.bera.test(resid(uecm3))

qcc::cusum(resid(uecm3))

recm3 <- recm(uecm3, case = 3)

summary(recm3)

bgtest(recm3)

bptest(recm3)

jarque.bera.test(resid(recm3))

qcc::cusum(resid(recm3))


################################ BOUND TEST IND_FEC

modelecm3 <- ardlBound(data = Modèle1 , formula = LN_PIB_HAB ~ LN_DEP_SANT_HAB + IND_FEC + DENS_TOT + TB_ACHV_PRIM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecm3$ARDL.model)

summary(modelecm3$ECM$EC.model)

modelecm3$bg

modelecm3$bp

modelecm3$lb

modelecm3$sp

############################### MODELE MORT_MAT

MCElt1.4 <- lm(LN_PIB_HAB ~ MORT_MAT + LN_DEP_SANT_HAB + DENS_TOT + TB_ACHV_FIPRM + Dummy89 + Dummy94)

check_collinearity(MCElt1.4)

summary(MCElt1.4)

res1.4 <- MCElt1.4$residuals

jarque.bera.test(res1.4)

bgtest(MCElt1.4)

bptest(MCElt1.4)

qcc::cusum(res1.4)

adf.test(res1.4)

########################## ARDL LONG TERME ET COURT TERME MORT_MAT

model4 <- ARDL::auto_ardl(LN_PIB_HAB ~ DENS_TOT + MORT_MAT + LN_DEP_SANT_HAB + TB_ACHV_PRIM + Dummy89 + Dummy94, data = Modèle1, max_order = c(2,2,2,2,2,2,2), selection = "BIC")

model4$top_orders

model4 <- ARDL::ardl(LN_PIB_HAB ~ DENS_TOT + MORT_MAT + LN_DEP_SANT_HAB + TB_ACHV_PRIM + Dummy89 + Dummy94, data = Modèle1, order = c(1,1,1,1,1,1,1))

summary(model4)

check_collinearity(model4)

plot(model4)

res4 <- model4$residuals

adf.test(res4)

pp.test(res4, type = "Z_tau")

kpss.test(res4)

jarque.bera.test(res4)

bgtest(model4)

bptest(model4)

qcc::cusum(res4)

uecm4 <-uecm(model4)

summary(uecm4)

check_collinearity(uecm4)

plot(uecm4)

bgtest(uecm4)

bptest(uecm4)

jarque.bera.test(resid(uecm4))

qcc::cusum(resid(uecm4))

recm4 <- recm(uecm4, case = 3)

summary(recm4)

multicollinearity(recm4)

bgtest(recm4)

bptest(recm4)

jarque.bera.test(resid(recm4))

qcc::cusum(resid(recm4))


######################## BOUND TEST MORT_MAT

modelecm4 <- ardlBound(data = Modèle1 , formula = LN_PIB_HAB ~ LN_DEP_SANT_HAB + MORT_MAT + DENS_TOT + TB_ACHV_PRIM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecm4$ARDL.model)

summary(modelecm4$ECM$EC.model)

modelecm4$bg

modelecm4$bp

modelecm4$lb

modelecm4$sp

############################################################### FEMME (Inégalité de Genre)

########################################################## FEMME ET CROISSANCE ECONOMIQUE

########################################### FEM_AGT_QUA

MCEltF2_1 <- lm(LN_PIB_HAB ~ DENS_TOT + FEM_ACTV + TAU_FER_ADO + PRO_FEM_AGT_QUA + TB_ACHV_FIPRM + TAU_MOR_INF + LN_DEP_SANT_HAB + ESP_VIE + MORT_MAT + IND_FEC + POP_15_64 + FBCF_CON + TB_ACHV_PRIM)

check_collinearity(MCEltF2_1)

summary(MCEltF2_1)

MCEltF2_2 <- update(MCEltF2_1, . ~ . - POP_15_64 - FEM_ACTV - TB_ACHV_FIPRM - TAU_FER_ADO - TAU_MOR_INF - IND_FEC - TB_ACHV_PRIM - MORT_MAT - FBCF_CON + Dummy89 + Dummy94)

check_collinearity(MCEltF2_2)

summary(MCEltF2_2)

par(mfrow=c(2,2))

plot(MCEltF2_2)

outlierTest(MCEltF2_2)

resF2_1 <- MCEltF2_2$residuals

jarque.bera.test(resF2_1)

bgtest(MCEltF2_2)

bptest(MCEltF2_2)

qcc::cusum(resF2_1)

adf.test(resF2_1)

kpss.test(resF2_1)

pp.test(resF2_1)

############### ARDL LONG TERME ET COURT TERME FEM_AGT_QUA

modelF2 <- ARDL::auto_ardl(LN_PIB_HAB ~ DENS_TOT + PRO_FEM_AGT_QUA + ESP_VIE + LN_DEP_SANT_HAB + Dummy89 + Dummy94, data = Modèle1, max_order = c(3,3,3,3,3,3,3), selection = "BIC")

modelF2$top_orders

modelF2 <- ARDL::ardl(LN_PIB_HAB ~ DENS_TOT + PRO_FEM_AGT_QUA + ESP_VIE + LN_DEP_SANT_HAB + Dummy89 + Dummy94, data = Modèle1, order = c(1,1,1,1,1,1,1))

summary(modelF2)

check_collinearity(modelF2)

plot(modelF2)

outlierTest(modelF2)

bgtest(modelF2)

bptest(modelF2)

jarque.bera.test(resid(modelF2))

qcc::cusum(resid(modelF2))

uecmF2 <-uecm(modelF2, case = 3)

check_collinearity(uecmF2)

summary(uecmF2)

outlierTest(uecmF2)

plot(uecmF2)

bgtest(uecmF2)

bptest(uecmF2)

jarque.bera.test(resid(uecmF2))

qcc::cusum(resid(uecmF2))

recmF2 <- recm(uecmF2, case = 3)

summary(recmF2)

multicollinearity(recmF2)

plot(recmF2)

bgtest(recmF2)

bptest(recmF2)

jarque.bera.test(resid(recmF2))

qcc::cusum(resid(recmF2))

######################## BOUND TEST FEM_AGT_QUA

modelecmF2 <- ardlBound(data = Modèle1 , formula = LN_PIB_HAB ~ DENS_TOT + PRO_FEM_AGT_QUA  + ESP_VIE + LN_DEP_SANT_HAB + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecmF2$ARDL.model)

summary(modelecmF2$ECM$EC.model)

modelecmF2$bg

modelecmF2$bp

modelecmF2$lb

modelecmF2$sp

############################################### TB_ACH_FIPRM

modelF3 <- ARDL::auto_ardl(LN_PIB_HAB ~ DENS_TOT + ESP_VIE + LN_DEP_SANT_HAB + TB_ACHV_FIPRM + Dummy89 + Dummy94, data = Modèle1, max_order = c(3,3,3,3,3,3,3), selection = "BIC" )

modelF3$top_orders

modelF3 <- ARDL::ardl(LN_PIB_HAB ~ DENS_TOT + ESP_VIE + LN_DEP_SANT_HAB + TB_ACHV_FIPRM + Dummy89 + Dummy94, data = Modèle1, order = c(1,1,1,1,1,1,1))

summary(modelF3)

check_collinearity(modelF3)

plot(modelF3)

outlierTest(modelF3)

bgtest(modelF3)

bptest(modelF3)

jarque.bera.test(resid(modelF3))

qcc::cusum(resid(modelF3))

uecmF3 <-uecm(modelF3, case = 3)

check_collinearity(uecmF3)

summary(uecmF3)

outlierTest(uecmF3)

plot(uecmF3)

bgtest(uecmF3)

bptest(uecmF3)

jarque.bera.test(resid(uecmF3))

qcc::cusum(resid(uecmF3))

recmF3 <- recm(uecmF3, case = 3)

summary(recmF3)

multicollinearity(recmF3)

plot(recmF3)

bgtest(recmF3)

bptest(recmF3)

jarque.bera.test(resid(recmF3))

qcc::cusum(resid(recmF3))


######################## BOUND TEST TB_ACHV_FIPRM

modelecmF3 <- ardlBound(data = Modèle1 , formula = LN_PIB_HAB ~ DENS_TOT + ESP_VIE + LN_DEP_SANT_HAB + TB_ACHV_FIPRM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecmF3$ARDL.model)

summary(modelecmF3$ECM$EC.model)

modelecmF3$bg

modelecmF3$bp

modelecmF3$lb

modelecmF3$sp

########################################################## FEMME ET INDICATEUR SANITAIRE


################################ FEMME ET ESP_VIE

############ AGENTS DE SANTE

MCEltF1 <- lm(ESP_VIE ~ DENS_TOT + PRO_FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + MORT_MAT +  IND_FEC + TAU_MOR_INF + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA, data = Modèle1)

check_collinearity(MCEltF1)

MCEltF2 <- update(MCEltF1, . ~ . - TAU_MOR_INF - MORT_MAT - TB_ACHV_FIPRM - TAU_URB - IND_FEC - POP_15_64 - FEM_ACTV - TB_ACHV_PRIM - DEP_EDUC - TB_MORT + Dummy85_90 + Dummy21,data = Modèle1) #### FEM_AGT_QUA

check_collinearity(MCEltF2)

summary(MCEltF2)

par(mfrow=c(2,2))

plot(MCEltF2)

outlierTest(MCEltF2)

resF2 <- MCEltF2$residuals

jarque.bera.test(resF2)

dwtest(MCEltF2)

bgtest(MCEltF2)

bptest(MCEltF2)

ncvTest(MCEltF2)

qcc::cusum(resF2)

######### EDUCATION

MCEltF22 <- update(MCEltF1, . ~ . - TAU_MOR_INF - MORT_MAT - PRO_FEM_AGT_QUA - TAU_URB - IND_FEC - POP_15_64 - FEM_ACTV - TB_ACHV_PRIM - DEP_EDUC - TB_MORT + Dummy85_89 + Dummy21,data = Modèle1) #### TB_ACV_FIPRM

check_collinearity(MCEltF22)

summary(MCEltF22)

par(mfrow=c(2,2))

plot(MCEltF22)

outlierTest(MCEltF22)

resF22 <- MCEltF22$residuals

jarque.bera.test(resF22)

dwtest(MCEltF22)

bgtest(MCEltF22)

bptest(MCEltF22)

ncvTest(MCEltF22)

qcc::cusum(resF22)

################################## IND_FEC ET FEMME

############ AGENTS DE SANTE

MCEltF3 <- lm(IND_FEC ~ DENS_TOT + PRO_FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + MORT_MAT +  ESP_VIE + TAU_MOR_INF + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM + INC_ANE + INC_PAL + INC_IRA)

check_collinearity(MCEltF3)

MCEltF4 <- update(MCEltF3, . ~ .  - POP_15_64 - DEP_EDUC - MORT_MAT - TAU_MOR_INF - FEM_ACTV - TB_MORT - TB_ACHV_FIPRM -TB_ACHV_PRIM - TAU_URB - ESP_VIE + Dummy85_90 + Dummy21) ######## FEM_AGT_QUA

check_collinearity(MCEltF4)

summary(MCEltF4)

par(mfrow=c(2,2))

plot(MCEltF4)

outlierTest(MCEltF4)

resF4 <- MCEltF4$residuals

jarque.bera.test(resF4)

dwtest(MCEltF4)

bgtest(MCEltF4)

bptest(MCEltF4)

ncvTest(MCEltF4)

qcc::cusum(resF4)

############ EDUCATION

MCEltF44 <- update(MCEltF3, . ~ . - PRO_FEM_AGT_QUA - POP_15_64 - DEP_EDUC - MORT_MAT - TAU_MOR_INF - FEM_ACTV - TB_MORT -TB_ACHV_PRIM - TAU_URB - ESP_VIE + Dummy85_90 + Dummy21)######### TB_ACHV_FIPRM

check_collinearity(MCEltF44)

summary(MCEltF44)

par(mfrow=c(2,2))

plot(MCEltF44)

outlierTest(MCEltF44)

resF44 <- MCEltF44$residuals

jarque.bera.test(resF44)

dwtest(MCEltF44)

bgtest(MCEltF44)

bptest(MCEltF44)

ncvTest(MCEltF44)

qcc::cusum(resF44)

################## TAU_MOR_INF ET FEMME

########## AGENTS DE SANTE

MCEltF5 <- lm(TAU_MOR_INF ~ DENS_TOT + PRO_FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + MORT_MAT +  ESP_VIE + IND_FEC + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA)

check_collinearity(MCEltF5)

MCEltF6 <- update(MCEltF5, . ~ .- FEM_ACTV - POP_15_64 - IND_FEC - TB_ACHV_PRIM - TB_ACHV_FIPRM - TAU_URB - ESP_VIE - TB_MORT - MORT_MAT - DEP_EDUC + Dummy85_90 + Dummy21)

check_collinearity(MCEltF6)

summary(MCEltF6)

par(mfrow=c(2,2))

plot(MCEltF6)

outlierTest(MCEltF6)

resF6 <- MCEltF6$residuals

jarque.bera.test(resF6)

dwtest(MCEltF6)

bgtest(MCEltF6)

bptest(MCEltF6)

ncvTest(MCEltF6)

qcc::cusum(resF6)

############## EDUCATION

MCEltF66 <- update(MCEltF5, . ~ .  - PRO_FEM_AGT_QUA - FEM_ACTV - POP_15_64 - IND_FEC - TAU_URB - ESP_VIE - TB_MORT - TB_ACHV_PRIM - MORT_MAT - DEP_EDUC + Dummy85_90 + Dummy21)

check_collinearity(MCEltF66)

summary(MCEltF66)

par(mfrow=c(2,2))

plot(MCEltF66)

outlierTest(MCEltF66)

resF66 <- MCEltF66$residuals

jarque.bera.test(resF66)

dwtest(MCEltF66)

bgtest(MCEltF66)

bptest(MCEltF66)

ncvTest(MCEltF66)

qcc::cusum(resF66)

################## MORT_MAT ET FEMME

############# AGENTS DE SANTE

MCEltF7 <- lm(MORT_MAT ~ DENS_TOT + PRO_FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + TAU_MOR_INF +  ESP_VIE + IND_FEC + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA)

check_collinearity(MCEltF7)

MCEltF8 <- update(MCEltF7, . ~ . - ESP_VIE - IND_FEC - FEM_ACTV - TAU_URB - TB_MORT - TB_ACHV_PRIM - TB_ACHV_FIPRM - POP_15_64 - TAU_MOR_INF - DEP_EDUC + Dummy85_89 + Dummy08 + Dummy21)

check_collinearity(MCEltF8)

summary(MCEltF8)

par(mfrow=c(2,2))

plot(MCEltF8)

outlierTest(MCEltF8)

resF8 <- MCEltF8$residuals

jarque.bera.test(resF8)

dwtest(MCEltF8)

bgtest(MCEltF8)

bptest(MCEltF8)

ncvTest(MCEltF8)

qcc::cusum(resF8)

########### EDUCATION

MCEltF88 <- update(MCEltF7, . ~ . - ESP_VIE - IND_FEC - PRO_FEM_AGT_QUA - FEM_ACTV - TAU_URB - TB_MORT - TB_ACHV_PRIM - POP_15_64 - TAU_MOR_INF - DEP_EDUC + Dummy85_89 + Dummy08 + Dummy21)

check_collinearity(MCEltF88)

summary(MCEltF88)

par(mfrow=c(2,2))

plot(MCEltF88)

outlierTest(MCEltF88)

resF88 <- MCEltF88$residuals

jarque.bera.test(resF88)

dwtest(MCEltF88)

bgtest(MCEltF88)

bptest(MCEltF88)

ncvTest(MCEltF88)

qcc::cusum(resF88)


################################################################################ Méthode d'Imputation utilisé pour les données manquantes


Modele0 <- read.table("clipboard", header=TRUE, dec=",", sep="\t", row.names = 1)#### Importation de la base (Copier la base du mod?le 1(valeur manquante) dans excel comme exemple puis ex?cuter la commande)

attach(Modele0)#### acc?s au variable de la base

names(Modele0)#### nom des variable de la base

library("mice") #### Chargement package mice

md.pattern(Modele0)#### Statistique valeur manquante 

imp <- mice(Modele0)#### Imputation par d?faut

imp#### affichage r?sultat imputation

r1 <- complete(imp, action = 1L)#### generation r1 avec imputation 1 

r2 <- complete(imp, action = 2L)#### generation r2 avec imputation 2

r3 <- complete(imp, action = 3L)#### generation r3 avec imputation 3

r4 <- complete(imp, action = 4L)#### generation r4 avec imputation 4

r5 <- complete(imp, action = 5L)#### generation r5 avec imputation 5

Modele0final <- ((r1+r2+r3+r4+r5)/5)##### cr?ation modele final moyenne des 5 mod?le imput?

row.names(Modele0final) <- row.names(Modele0)##### colonne 0 mod?le1final = colonne p?riode mod?le1

attach(Modele0final)#### acc?s au variable de la base

names(Modele0final)#### nom des variable de la base

View(Modele0final)


################################################################### AFTER

dLN_PRIM <- diff(LN_PRIM)

ddLN_PRIM <- diff(dLN_PRIM)

dLN_SEC <- diff(LN_SEC)

dLN_TER <- diff(LN_TER)

model3LN_PRIM <- ur.df(dLN_PRIM, type = "none", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_PRIM)#### Affichage r?sultats du modele

trend00 <- as.numeric(time(LN_PRIM))#### Extraction de la tendance de la s?rie

LN_PRIMts <- lm(LN_PRIM~trend00)#### Regression de la s?rie sur sa tendance 

summary.lm(LN_PRIMts)#### affichage r?sultat de la regression

LN_PRIM_TS <- LN_PRIMts$residuals#### extraction du r?sidus de la regression

model3LN_PRIM_TS  <- ur.df(LN_PRIM_TS, type = "trend", lags = 0)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3LN_PIB_TS )#### Affichage r?sultats

model3dLN_PIB <- ur.df(dLN_PIB, type = "trend", lags = 2)#### Test de dickey fuller avec tendance avec constante sur la variable

summary(model3dLN_PIB)#### Affichage r?sultats

model2dLN_PIB <- ur.df(dLN_PIB, type = "drift", lags = 2)#### Test de dickey fuller sans tendance avec constante sur la variable

summary(model2dLN_PIB)#### Affichage r?sultats

pp.test(dLN_PIB)

kpss.test(dLN_PIB)


MCElt777 <- lm(LN_SEC ~ DENS_TOT + LN_DEP_SANT +  ESP_VIE + TB_ACHV_PRIM)

check_collinearity(MCElt777)


############### ARDL LONG TERME ET COURT TERME ESP_VIE (SECTEUR TERTIAIRE)

model777 <- ARDL::auto_ardl(LN_TER ~ DENS_TOT + ESP_VIE + LN_DEP_SANT + TB_ACHV_PRIM + Dummy89 + Dummy94, data = Modèle1, max_order = c(2,2,2,2,2,2,2), selection = "BIC")

model777$top_orders

model777 <- ARDL::ardl(LN_TER ~ DENS_TOT + ESP_VIE + LN_DEP_SANT + TB_ACHV_PRIM + Dummy89, data = Modèle1, order = c(1,1,1,1,1,1))

summary(model777)

check_collinearity(model777)

outlierTest(model777)

par(mfrow=c(2,2))

plot(model777)

bgtest(model777)

bptest(model777)

jarque.bera.test(resid(model777))

qcc::cusum(resid(model777))

uecm777 <-uecm(model777, case = 3)

check_collinearity(uecm777)

summary(uecm777)

bgtest(uecm777)

bptest(uecm777)

jarque.bera.test(resid(uecm777))

qcc::cusum(resid(uecm777))

recm777 <- recm(uecm777, case = 3)

summary(recm777)

multicollinearity(recm777)

plot(recm777)

bgtest(recm777)

bptest(recm777)

jarque.bera.test(resid(recm777))

qcc::cusum(resid(recm777))

######################## BOUND TEST ESP_VIE

modelecm777 <- ardlBound(data = Modèle1 , formula = LN_TER ~ LN_DEP_SANT + ESP_VIE + DENS_TOT + TB_ACHV_PRIM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecm777$ARDL.model)

summary(modelecm777$ECM$EC.model)

modelecm777$bg

modelecm777$bp

modelecm777$lb

modelecm777$sp


############### ARDL LONG TERME ET COURT TERME ESP_VIE (SECTEUR SECONDAIRE)

model77 <- ARDL::auto_ardl(LN_SEC ~ DENS_TOT + ESP_VIE + LN_DEP_SANT + TB_ACHV_PRIM + Dummy89 + Dummy94, data = Modèle1, max_order = c(2,2,2,2,2,2,2), selection = "BIC")

model77$top_orders

model77 <- ARDL::ardl(LN_SEC ~ DENS_TOT + ESP_VIE + LN_DEP_SANT + TB_ACHV_PRIM + Dummy89 + Dummy06 + Dummy15, data = Modèle1, order = c(1,1,1,1,1,1,1,1))

summary(model77)

check_collinearity(model77)

outlierTest(model77)

par(mfrow=c(2,2))

plot(model77)

bgtest(model77)

bptest(model77)

jarque.bera.test(resid(model77))

qcc::cusum(resid(model77))

uecm77 <-uecm(model77, case = 3)

check_collinearity(uecm77)

summary(uecm77)

bgtest(uecm77)

bptest(uecm77)

jarque.bera.test(resid(uecm77))

qcc::cusum(resid(uecm77))

recm77 <- recm(uecm77, case = 3)

summary(recm77)

multicollinearity(recm77)

plot(recm77)

bgtest(recm77)

bptest(recm77)

jarque.bera.test(resid(recm77))

qcc::cusum(resid(recm77))

######################## BOUND TEST ESP_VIE

modelecm77 <- ardlBound(data = Modèle1 , formula = LN_SEC ~ LN_DEP_SANT + ESP_VIE + DENS_TOT + TB_ACHV_PRIM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecm77$ARDL.model)

summary(modelecm77$ECM$EC.model)

modelecm77$bg

modelecm77$bp

modelecm77$lb

modelecm77$sp


############### ARDL LONG TERME ET COURT TERME ESP_VIE (SECTEUR PRIMAIRE)

model7 <- ARDL::auto_ardl(LN_PRIM ~ DENS_TOT + ESP_VIE + LN_DEP_SANT + TB_ACHV_PRIM, data = Modèle1, max_order = c(2,2,2,2,2), selection = "BIC")

model7$top_orders

model7 <- ARDL::ardl(LN_PRIM ~ DENS_TOT + ESP_VIE + LN_DEP_SANT + TB_ACHV_PRIM, data = Modèle1, order = c(1,1,1,1,1))

summary(model7)

check_collinearity(model7)

outlierTest(model7)

par(mfrow=c(2,2))

plot(model7)

bgtest(model7)

bptest(model7)

jarque.bera.test(resid(model7))

qcc::cusum(resid(model7))

uecm7 <-uecm(model7, case = 3)

check_collinearity(uecm7)

summary(uecm7)

bgtest(uecm7)

bptest(uecm7)

jarque.bera.test(resid(uecm7))

qcc::cusum(resid(uecm7))

recm7 <- recm(uecm7, case = 3)

summary(recm7)

multicollinearity(recm7)

plot(recm7)

bgtest(recm7)

bptest(recm7)

jarque.bera.test(resid(recm7))

qcc::cusum(resid(recm7))

######################## BOUND TEST ESP_VIE

modelecm7 <- ardlBound(data = Modèle1 , formula = LN_SEC ~ LN_DEP_SANT + ESP_VIE + DENS_TOT + TB_ACHV_PRIM + Dummy89 + Dummy94, case = 3 , max.p = 1, max.q = 1, ECM = T)

summary(modelecm7$ARDL.model)

summary(modelecm7$ECM$EC.model)

modelecm7$bg

modelecm7$bp

modelecm7$lb

modelecm7$sp


##################################################### CROISSANCE PIB ET POP (Non concluant)

MCEltP77 <- lm(CRO_POP ~ DENS_TOT + ESP_VIE + FEM_AGT_QUA + POP_15_64 + FEM_ACTV + TAU_URB + MORT_MAT +  IND_FEC + TAU_MOR_INF + TB_MORT + DEP_EDUC + TB_ACHV_PRIM + TB_ACHV_FIPRM +  INC_ANE + INC_PAL + INC_IRA + CRO_PIB_HAB + CRO_PIB, data = Modèle1)

check_collinearity(MCEltP.1)

MCEltP771 <- update(MCEltP77, . ~ . - TAU_MOR_INF - MORT_MAT - INC_PAL - TB_ACHV_PRIM - FEM_AGT_QUA - TAU_URB - IND_FEC - POP_15_64 - FEM_ACTV - TB_ACHV_FIPRM - DEP_EDUC - TB_MORT + Dummy85_89 + Dummy21 - CRO_PIB,data = Modèle1)

check_collinearity(MCEltP771)

summary(MCEltP771)

par(mfrow=c(2,2))

plot(MCEltP771)

outlierTest(MCEltP771)

resP771 <- MCEltP771$residuals

jarque.bera.test(resP771)

dwtest(MCEltP771)

bgtest(MCEltP771)

bptest(MCEltP771)

ncvTest(MCEltP771)

qcc::cusum(resP771)

resettest(MCEltP771)


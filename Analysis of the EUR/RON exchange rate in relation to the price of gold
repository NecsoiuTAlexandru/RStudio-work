# Tema Proiectului -------Previzionarea cursului de schimb RON/EUR și analiza relației cu prețul aurului în perioada 2018–2025--------
#Ksenia Paskari
#Necsoiu Toader Alexandru
#Nistor Cosmin


#Problema economica pe care se bazeaza acest proiect:
#Cum a evoluat cursul de schimb RON/EUR în ultimii 7 ani și în ce măsură a fost influențat de dinamica prețului aurului
#atât în contextul instabilității economice globale , cât și naționale (pandemie, inflație, razboiul din Ucraina și alegerile prezidentiale de anul acesta)?
#Am luat ca indici cursul valutar euro/lei si pretul aurului in euro
eur<-read.csv(file="curs euro.csv" , header=T , sep=";")
library(xts)
library(tseries)
library(ggplot2)
library(TSstudio)
library(forecast)
library(lubridate)
library(urca)
library(rugarch)
library(FinTS)
library(lmtest)
library(vars)

eur$Date <- mdy(eur$Date)

# Analiza pretului aurului ------------------------------------------------


ts_aur <- ts(na.omit(eur$Price.Aur.EURO), frequency = 261, start = c(2018, 1))
autoplot(ts_aur) + ggtitle("Evoluția pretului aurului") + xlab("Timp") + ylab("RON")
#observam iarasi un trend crescator , fara sezonalitate insa

summary(ur.df(ts_aur, type = "drift", selectlags = "AIC"))
acf(ts_aur, lag.max = 40, main = "ACF – Pret Aur ")
pacf(ts_aur, lag.max = 40, main = "PACF – Pret Aur  ")
#Lagurile scad foarte lent si putin ceea ce ne indica clar ca nu e stationara
#Avem de asemenea in ACF lagul 1 foarte mare iar restul nesemnificative 
#Seria este nestationara , doar cu trend deci va trebui sa aplicam o diferentiere


summary(ur.kpss(ts_aur))
#Acest lucru reiese si din testul KPSS care arata clar ca este stationara cu p-val crit d 17.67
PP.test(ts_aur)
#Si din testul Phillips-Perron cu p value 98% care clar este peste 5 % , deci respingem H0->nestationara

diff_aur <- diff(ts_aur)
adf.test(na.omit(diff_aur)) 
#P-value este 1%<5% ceea ce ete bine pentru modelul nostru de testare
#am transformat seria din nestationara in stationara
#A fost nevoie doar de o diferentiere
# Modelare ARIMA automată (autoarima alege d=1 automat)#Modelul cel mai optim

acf(diff_aur, lag.max = 40, main = "ACF – Pret Aur diferențiat")
pacf(diff_aur, lag.max = 40, main = "PACF – Pret Aur diferențiat")

ggtsdisplay(diff_aur)
#Reziduurile în timp fluctueaza in jurul valorii 0 , cu exceptia unor spike-uri in 2022 ,2022 si 2025
#Putem observa att in ACF cat si PACF ca avem un numar relativ mare de laguri care depasesc intervalul , nu mult dar il depasesc

summary(ur.df(diff_aur, type = "drift", selectlags = "AIC"))
summary(ur.kpss(diff_aur))
PP.test(diff_aur)
#Din toate testele reiese clad staionara dupa diferentiere

training_aur <- window(ts_aur, start = c(2018, 1), end = c(2025, 24))  
test_aur <- window(ts_aur, start = c(2025, 25))


##########OBSERVATIE FOARTE IMPORTANTA-###########
##### AM REALIZAT TOCMAI DUPA TERMINAREA ANALIZEI CA SETUL DE DATE DE TESTARE E PREA MIC
##### COMPARATIV CU PERIOADA DE 7 ANI SI CV ANALIZATA , DECI E POSIBIL CA SETUL DE TESTARE SA NU FI CUPRINS
##### FOARTE BINE TOATE SCHIMBARILE SI DEZECHILIBRELE DIN SETUL DE DATE , MODELELE NU AU REUSIT CEL MAI PROBABIL
#### SA CAPTEZE SI SA ANALIZEZE BINE TOATA INFORMATIA . NE CEREM SCUZE (E VALABIL ATAT PENTRU CURSUL EURO/RON CAT SI PENTRU AUR)



fit_aur <- holt(training_aur, h = length(test_aur))
accuracy(fit_aur, test_aur)
summary(fit_aur)
checkresiduals(fit_aur)
#alpha = 0.9637:foarte aproape de 1 , deci modelul pune accent puternic pe ultima observație , reacționează rapid la schimbări.
#Putem observa ca nu exista autocorelare intre reziduri
set.seed(1234)

autoplot(ts_aur, series = "Curs EUR/RON") +
  autolayer(fit_aur$mean, series = "Prognoză Holt") +
  autolayer(test_aur, series = "Valori reale", color = "red") +
  ggtitle("Prognoză Holt vs. evoluție reală EUR/RON") +
  xlab("Timp") + ylab("Curs EUR/RON") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw()
#Modelul Holt prefigureaza o crestere a pretului aurului pana aproape de 3000 de euro

arima010_aur <- Arima(training_aur, order = c(0,1,0), include.constant = TRUE)
coeftest(arima010_aur)
arima011_aur <- Arima(training_aur, order = c(0,1,1), include.constant = TRUE)
coeftest(arima011_aur)
arima012_aur <- Arima(training_aur, order = c(0,1,2), include.constant = TRUE)
coeftest(arima012_aur)
arima013_aur <- Arima(training_aur, order = c(0,1,3), include.constant = TRUE)
coeftest(arima013_aur)
arima110_aur <- Arima(training_aur, order = c(1,1,0), include.constant = TRUE)
coeftest(arima110_aur)
arima111_aur <- Arima(training_aur, order = c(1,1,1), include.constant = TRUE)
coeftest(arima111_aur)
arima112_aur <- Arima(training_aur, order = c(1,1,2), include.constant = TRUE)
coeftest(arima112_aur)
arima113_aur <- Arima(training_aur, order = c(1,1,3), include.constant = TRUE)
coeftest(arima113_aur)
arima210_aur <- Arima(training_aur, order = c(2,1,0), include.constant = TRUE)
coeftest(arima210_aur)
arima211_aur <- Arima(training_aur, order = c(2,1,1), include.constant = TRUE)
coeftest(arima211_aur)
arima310_aur <- Arima(training_aur, order = c(3,1,0), include.constant = TRUE)
coeftest(arima310_aur)
arima214_aur <- Arima(training_aur, order = c(2,1,4), include.constant = TRUE)
coeftest(arima214_aur)

summary(arima010_aur)
summary(arima011_aur) 
summary(arima012_aur)
summary(arima013_aur)
summary(arima110_aur)
summary(arima111_aur)
summary(arima112_aur)
summary(arima113_aur)
summary(arima210_aur)
summary(arima211_aur)
summary(arima310_aur)
summary(arima214_aur)

AIC(arima010_aur)
AIC(arima011_aur)
AIC(arima012_aur)
AIC(arima013_aur)
AIC(arima110_aur)
AIC(arima111_aur)
AIC(arima112_aur)
AIC(arima113_aur)
AIC(arima210_aur)
AIC(arima211_aur)
AIC(arima310_aur)
AIC(arima214_aur)
auto.arima(training_aur,d=1, seasonal=FALSE) 
#In urma compararii intre ARIMA210 si ARIMA214 am ajuns la concluzia ca cel mai perfomant model dpdv al AIC , nr de coef semnificativi (ar si am)
#drift si RMSE este ARIMA214. Avem insa 2 coef ma1 si ma2 care sunt nesemnificativi , insa aceste nu afecteaza modelul nostru deoarece nu vom avea autocorelare in reziduri
arima214_aur_acuratete<-arima214_aur%>%forecast::forecast(h=63)
summary(arima214_aur_acuratete)

#In urma testului Ljung-Box 49% respingem ipoteza nula si accepta H1 , adica pentru arima210 nu avem autocorelare intre reziduri
arima214_aur%>%forecast(h=63)%>%autoplot()
checkresiduals(arima214_aur)
#Testam care laguri sunt <5% pentru Box-Pierce
Box.test(residuals(arima214_aur), lag=1)#nu este
Box.test(residuals(arima214_aur), lag=2) #nu este
Box.test(residuals(arima214_aur), lag=3) #nu este
Box.test(residuals(arima214_aur), lag=4) #nu este
Box.test(residuals(arima214_aur), lag=7) #nu este
Box.test(residuals(arima214_aur), lag=8) #nu este
Box.test(residuals(arima214_aur), lag=10) #nu este
Box.test(residuals(arima214_aur), lag=11) #nu este
Box.test(residuals(arima214_aur), lag=12) #nu este
Box.test(residuals(arima214_aur), lag=13) #nu este
Box.test(residuals(arima214_aur), lag=14) #nu este
Box.test(residuals(arima214_aur), lag=30) #nu este
Box.test(residuals(arima214_aur), lag=40) #nu este
#Din alugrile analizate nu se observa autocorelare

#Testam care laguri sunt <5%
Box.test(residuals(arima214_aur), lag=1, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=2, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=3, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=4, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=5, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=6, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=7, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=8, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=9, type="Lj") #nu este
Box.test(residuals(arima214_aur), lag=10, type="Lj")#nu este
Box.test(residuals(arima214_aur), lag=20, type="Lj")#nu este
Box.test(residuals(arima214_aur), lag=30, type="Lj")#nu este
Box.test(residuals(arima214_aur), lag=40, type="Lj")#nu este
Box.test(residuals(arima214_aur), lag=50, type="Lj")#nu este
Box.test(residuals(arima214_aur), lag=60, type="Lj")#nu este
Box.test(residuals(arima214_aur), lag=100, type="Lj")#nu este
Box.test(residuals(arima214_aur), lag=200, type="Lj")#nu este

ArchTest(residuals(arima214_aur), lags=1) 
ArchTest(residuals(arima214_aur), lags=2) 
ArchTest(residuals(arima214_aur), lags=3) 
ArchTest(residuals(arima214_aur), lags=4) 
ArchTest(residuals(arima214_aur), lags=5) 
ArchTest(residuals(arima214_aur), lags=6) 
ArchTest(residuals(arima214_aur), lags=7) 
ArchTest(residuals(arima214_aur), lags=8) 
ArchTest(residuals(arima214_aur), lags=9) 
ArchTest(residuals(arima214_aur), lags=10) 
#Modelul prezinta heteroscedasticitate in toate lagurile , deci vom aplica GARCH

autoplot(ts_aur, series = "Seria reală") +
  autolayer(arima214_aur_acuratete, series = "ARIMA(2,1,4)", PI = TRUE) +
  ggtitle("Prognoza preț aur cu modelul ARIMA(2,1,0)") +
  xlab("Timp") + ylab("Curs EUR/RON") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw()

len_aur <- min(length(arima214_aur$residuals), length(fit_aur$residuals))
dm.test(arima214_aur$residuals[1:len_aur], fit_aur$residuals[1:len_aur], h=1)
#Resp ip nula.Ambele modele au cam aceleasi performante
#ARIMA este totuși mai sofisticat și captează mai bine dinamica dacă orizontul este mai lung (7, 30, 63 zile).
dm.test(arima214_aur_acuratete$mean, fit_aur$mean, h=1)
#Testul Diebold-Mariano indică o diferență semnificativă statistic între erorile de prognoză ale modelului ARIMA(2,1,4) și cele ale modelului Holt, în favoarea modelului ARIMA.
#Valoarea negativă a statisticii DM (-10.746) → indică faptul că ARIMA are o eroare de prognoză semnificativ mai mică decât Holt.

autoplot(ts_aur, series = "Seria reală") +
  autolayer(fit_aur, series = "Holt", PI = TRUE, color = "green4", size = 1.3) +
  autolayer(arima214_aur_acuratete, series = "ARIMA(2,1,4)", PI = TRUE, color = "red3", size = 0.8) +
  guides(colour = guide_legend(title = "Forecast")) +
  xlab("Zile") +
  ylab("Preț aur (EUR)") +
  ggtitle("Forecasts finale ale prețului la aur") +
  theme_bw()


class(fit_aur)
fit_aur$mean - arima214_aur_acuratete$mean
#Cele 2 modele se suprapun deoarece diferentele de predictie sunt intre -1 si 6 , ceea ce nu este asa de mult comparativ cu pretul acestuia
#Modelul prezinta heteroscedasticitate mare in toate lagurile , deci vom aplica GARCH



# Analiza volatilitatii aurului GARCH -------------------------------------



aur_returns <- diff(eur$Price.Aur.EURO)
aur_returns <- na.omit(aur_returns)
aur_dates <- eur$Date[-1]  # eliminăm prima zi pierdută prin diferențiere
aur_returns_xts <- xts(aur_returns, order.by = aur_dates)

# Se specifică modelul GARCH(1,1) cu o componentă AR(1) pentru media condiționată.
# Alegerea modelului AR(1) este justificată prin existența unei autocorelări slabe în valorile diferențiate.
spec2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 4), include.mean = TRUE),
  distribution.model = "norm" )  # Se utilizează distribuția normală pentru reziduuri

# Estimarea modelului pe seria diferențiată
garch_model2 <- ugarchfit(spec = spec2, data = aur_returns_xts)

# Afișarea rezultatelor modelului
show(garch_model2)
volatility <- sigma(garch_model2)
#toti coef semnificativi cu exceptia a ma3 alpha+beta=99% , avem clar volatilitate semnificativa
#Ljung-Box pe reziduuri: p > 0.05 → nu există autocorelare semnificativă
#Ljung-Box pe pătrate reziduale: p > 0.05 → nu există autocorelare a volatilității → bun
#ARCH LM: p > 0.05 → modelul a capturat heteroscedasticitatea
#Sign bias test: doar bias-ul negativ e ușor semnificativ (p ≈ 0.09), dar per ansamblu nu avem probleme serioase
#toate valorile < 0.35–0.75 → coeficienții sunt stabili
#Joint: 2.53 < 2.54 (critic la 5%) → la limită stabil
#Pearson goodness-of-fit: p < 0.01 → nu se potrivește perfect cu distribuția normală
plot(volatility, type = "l", col = "blue", lwd = 2,
     main = "Volatilitatea estimată a pretului aurului (GARCH(1,1))",
     ylab = expression(sigma[t]), xlab = "Timp")
#Randamentele aurului au o medie pozitivă și semnificativă ,există un trend slab ascendent
#șocurile asupra prețului aurului (ex: geopolitică, inflație, crize) au efecte de lungă durată,adica dupa o criza
#economica in care aurul este folosit ca inlocuitor al valutelor , tinde sa si mentia pretul la care a crescut constant,sau chiar sa creasca
#comportament tipic pentru active de refugiu
#Reacțiile la șocuri negative ar putea fi asimetrice, ceea ce este normal:
#Investitorii reacționează diferit la incertitudine – cresc cererea de aur în crize


# Analiza cursul EURO-RON -------------------------------------------------

set.seed(1234)

ts_eur <- ts(na.omit(eur$Pret.EURO.LEI), frequency = 261, start = c(2018, 1))
#Am luat un an vand 263 de zile-in care preturile sunt listate (zile lucratoare si fara sarbatori)
#Se observa de alungul anilor o crestere continua a cursului valutar EURO/LEI . Se observa deci un trend crescator ,insa fara sezonalitate

autoplot(ts_eur) + ggtitle("Evoluția cursului EUR/RON") + xlab("Timp") + ylab("RON")
#Putem observa ca trendul este unul crescator , deci seria este din start nestationara
#Putem observa insa ca nu are sezonalitate , ci doar trend
#Leul romanesc s-a depreciat treptat fata de euro

acf(ts_eur, lag.max = 40, main = "ACF – Pret Aur ")
pacf(ts_eur, lag.max = 40, main = "PACF – Pret Aur ")
#Se obserca clar ca laguruile in ACF descresc foarte putin si lent
#pentru PACF singurul lag care depaseste intervalul este primul

summary(ur.df(ts_eur, type = "drift", selectlags = "AIC"))
#Seria este nestationara , doac cu trend deci va trebui sa aplicam o diferentiere
summary(ur.kpss(ts_eur))
#Acest lucru reiese si din testul KPSS care arata ckar ca este stationara cu p-val crit d 18.78
PP.test(ts_eur)
#Si din testul Phillips-Perron cu p value 41% care clar este peste 5 % , deci respingem H0->nestationara

diff_eur <- diff(ts_eur)

eur$diff_EUR <- c(NA, diff(eur$Pret.EURO.LEI))
adf.test(na.omit(eur$diff_EUR)) 
#P-value este 1%<5% ceea ce ete bine pentru modelul nostru de testare
#am transformat seria din nestationara in stationara
#A fost nevoie doar de o diferentiere
# Modelare ARIMA automată (autoarima alege d=1 automat)#Modelul cel mai optim

acf(diff_eur, lag.max = 40, main = "ACF – Curs Euro/RON diferențiat")
pacf(diff_eur, lag.max = 40, main = "PACF – Curs Euro/RON diferențiat")
#Se observă un spike clar la lag 1 negativ și câteva altele relativ apropiate de limite.
#Asta indică o posibilă componență AR(1) semnificativă, cu efecte marginale la laguri mai îndepărtate.
#Nu apar structuri persistente de autocorelare, ceea ce indică faptul că seria este aproape white noise după diferențiere.
#Primul lag este semnificativ, restul lagurilor sunt în mare parte în interiorul benzilor albastre, adică nu semnificativ autocorelate.
#Se poate aplica un AR1 sau 2
ggtsdisplay(diff_eur)
#Reziduurile în timp fluctueaza in jurul valorii 0 , cu exceptia unor spike-uri in 2022 si 2019
#Majoritatea lag-urilor ACF sunt în interiorul benzilor albastre , nu există autocorelare semnificativă doar cateva spike-uri
#Acelasi lucu la PACF ca la ACF

summary(ur.df(diff_eur, type = "drift", selectlags = "AIC"))
summary(ur.kpss(diff_eur))
PP.test(diff_eur)
#Din toate testele reiese clad staionara dupa diferentiere

train <- window(ts_eur, start = c(2018, 1), end = c(2025, 24))
test <- window(ts_eur, start = c(2025, 25))
fit <- holt(train, h = length(test))
accuracy(fit, test)
summary(fit)

train_dif1<-train%>%diff(lag=1)
train%>%diff(lag=1)%>%ggtsdisplay()
ggAcf(train_dif1)+theme_bw()
ggPacf(train_dif1)+theme_bw()
#Putem observa cateva laguri care depasesc intervalul de autocorelare, asa ca vom aplica arima
end(ts_eur)

summary(ur.df(train_dif1, type='none', selectlags=c("AIC"))) #stationar
summary(ur.df(train_dif1, type="trend", selectlags = c("AIC"))) #stationar
summary(ur.df(train_dif1, type="drift", selectlags = c("AIC"))) #stationar

autoplot(ts_eur, series = "Curs EUR/RON") +
  autolayer(fit$mean, series = "Prognoză Holt") +
  autolayer(test, series = "Valori reale", color = "red") +
  ggtitle("Prognoză Holt vs. evoluție reală EUR/RON") +
  xlab("Timp") + ylab("Curs EUR/RON") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw()


#Modelul Holt oferă o prognoză extrem de precisă pentru cursul EUR/RON, cu o eroare absolută medie
#de doar 3–4 bani și o eroare procentuală medie de sub 1%. Acest lucru indică faptul că evoluția cursului
#a fost destul de liniară în perioada test, fără fluctuații bruște sau comportament haotic.

arima010 <- Arima(train, order = c(0,1,0), include.constant = TRUE)
coeftest(arima010)
arima011 <- Arima(train, order = c(0,1,1), include.constant = TRUE)
coeftest(arima011)
arima012 <- Arima(train, order = c(0,1,2), include.constant = TRUE)
coeftest(arima012)
arima013 <- Arima(train, order = c(0,1,3), include.constant = TRUE)
coeftest(arima013)
arima110 <- Arima(train, order = c(1,1,0), include.constant = TRUE)
coeftest(arima110)
arima111 <- Arima(train, order = c(1,1,1), include.constant = TRUE)
coeftest(arima111)
arima112 <- Arima(train, order = c(1,1,2), include.constant = TRUE)
coeftest(arima112)
arima113 <- Arima(train, order = c(1,1,3), include.constant = TRUE)
coeftest(arima113)
arima210 <- Arima(train, order = c(2,1,0), include.constant = TRUE)
coeftest(arima210)
arima211 <- Arima(train, order = c(2,1,1), include.constant = TRUE)
coeftest(arima211)
arima310<- Arima(train, order = c(3,1,0), include.constant = TRUE)
coeftest(arima310)
arima312<- Arima(train, order = c(3,1,2), include.constant = TRUE)
coeftest(arima312)


summary(arima010)
summary(arima011) 
summary(arima110)
summary(arima111)
summary(arima012)
summary(arima210)
summary(arima211)
summary(arima310)
summary(arima312)

AIC(arima010)
AIC(arima011)
AIC(arima110)
AIC(arima111)
AIC(arima210)
AIC(arima211)
AIC(arima310)
AIC(arima312)
#Cel mai buna AIC il are arima312 insa cele mai interpretabile si performate modele cu coef mari sunt ARIMA(2,1,0)si ARIMA(2,1,1)
auto.arima(train,d=1, seasonal=FALSE) 

arima210_acuratete<-arima210%>%forecast::forecast(h=63)
summary(arima210_acuratete)

autoplot(ts_eur, series = "Seria reală") +
  autolayer(arima210_acuratete, series = "ARIMA(2,1,0)", PI = TRUE) +
  ggtitle("Prognoza cursului EUR/RON cu modelul ARIMA(2,1,0)") +
  xlab("Timp") + ylab("Curs EUR/RON") +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw()
#Modelul ARIMA(2,1,0) cu drift s-a dovedit a fi cel mai potrivit pentru prognoza evoluției cursului EUR/RON, 
#înregistrând cele mai bune performanțe statistice dintre modelele testate. Alegerea acestuia este susținută de valoarea minimă a criteriului AIC (-14237.22), 
#precum și de semnificația statistică ridicată a coeficienților autoregresivi și ai mediei mobile, ceea ce indică o capacitate ridicată de captare a dinamicii seriei.
#Driftul, deși nesemnificativ statistic, reflectă un trend ușor ascendent, compatibil cu contextul economic analizat. Valorile scăzute ale erorilor de prognoză

arima210%>%forecast(h=63)%>%autoplot()
checkresiduals(arima210)

#Testam care laguri sunt <5% pentru Box-Pierce
Box.test(residuals(arima210), lag=1)#nu este
Box.test(residuals(arima210), lag=2) #nu este
Box.test(residuals(arima210), lag=3) #nu este
Box.test(residuals(arima210), lag=4) #nu este
Box.test(residuals(arima210), lag=5) #nu este
Box.test(residuals(arima210), lag=6) #nu este
Box.test(residuals(arima210), lag=7) #nu este
Box.test(residuals(arima210), lag=8) #nu este
Box.test(residuals(arima210), lag=9) #este
Box.test(residuals(arima210), lag=10) #nu este
Box.test(residuals(arima210), lag=11) # este
Box.test(residuals(arima210), lag=12) #nu este
Box.test(residuals(arima210), lag=13) #nu este
Box.test(residuals(arima210), lag=14) #nu este
Box.test(residuals(arima210), lag=30) #nu este
Box.test(residuals(arima210), lag=40) #este
#Putem observa un numar de autocorelari la testul Box-Pierce

#Testam care laguri sunt <5%
Box.test(residuals(arima210), lag=1, type="Lj") #nu este
Box.test(residuals(arima210), lag=2, type="Lj") #nu este
Box.test(residuals(arima210), lag=3, type="Lj") #nu este
Box.test(residuals(arima210), lag=4, type="Lj") #nu este
Box.test(residuals(arima210), lag=5, type="Lj") #nu este
Box.test(residuals(arima210), lag=6, type="Lj") #nu este
Box.test(residuals(arima210), lag=7, type="Lj") #nu este
Box.test(residuals(arima210), lag=8, type="Lj") #nu este
Box.test(residuals(arima210), lag=9, type="Lj") #este
Box.test(residuals(arima210), lag=10, type="Lj") #nu este
Box.test(residuals(arima210), lag=15, type="Lj") #nu este
Box.test(residuals(arima210), lag=20, type="Lj") #este
Box.test(residuals(arima210), lag=30, type="Lj") #este
Box.test(residuals(arima210), lag=40, type="Lj") #este
Box.test(residuals(arima210), lag=50, type="Lj") # este
Box.test(residuals(arima210), lag=60, type="Lj") # este
Box.test(residuals(arima210), lag=100, type="Lj") #este
Box.test(residuals(arima210), lag=200, type="Lj") #nu este

ArchTest(residuals(arima210), lags=1) 
ArchTest(residuals(arima210), lags=2) 
ArchTest(residuals(arima210), lags=3) 
ArchTest(residuals(arima210), lags=4) 
ArchTest(residuals(arima210), lags=5) 
ArchTest(residuals(arima210), lags=6) 
ArchTest(residuals(arima210), lags=7) 
ArchTest(residuals(arima210), lags=8) 
ArchTest(residuals(arima210), lags=9) 
ArchTest(residuals(arima210), lags=10) 
#Modelul prezinta heteroscedasticitate in toate lagurile , deci vom aplica GARCH(1,1)

len <- min(length(arima210$residuals), length(fit$residuals))
dm.test(arima210$residuals[1:len], fit$residuals[1:len], h=1)
#Ambele modele au cam aceleasi performante
dm.test(arima210_acuratete$mean, fit$mean, h=1)
#Testul Diebold-Mariano aplicat pe erorile de prognoză generate de modelele ARIMA(2,1,0) și Holt 
#a indicat o diferență semnificativă statistic (DM = 9.12, p < 0.0001), confirmând superioritatea modelului 
#ARIMA în ceea ce privește acuratețea prognozei pe termen mediu (63 de zile lucrătoare). 
#Acest rezultat susține alegerea ARIMA ca model final de previziune.

autoplot(ts_eur, series = "Seria reală") +
  autolayer(fit, series = "Holt", PI = TRUE) +
  autolayer(arima210_acuratete, series = "ARIMA(2,1,0)", PI = TRUE) +
  guides(colour = guide_legend(title = "Forecast")) +
  xlab("Zile") +
  ylab("Curs EUR/RON") +
  ggtitle("Forecasts finale ale cursului EUR/RON") +
  theme_bw()

head(fit$mean - arima210_acuratete$mean)

#Diferențele dintre prognozele modelelor Holt și ARIMA(2,1,0) sunt de ordinul 0,00005 bani, ceea ce le face aproape imposibil de distins vizual. 
#Totuși, aceste diferențe, deși minuscule, sunt sistematic în favoarea modelului ARIMA, ceea ce a dus la un rezultat semnificativ statistic în testul Diebold-Mariano.
#Astfel, modelul ARIMA este superior din punct de vedere al preciziei,
#chiar dacă ambele modele oferă prognoze aproape identice la nivel grafic.


# ---- Modelarea Volatilității Cursului EUR/RON folosind GARCH(1,1) ----

# Pentru a surprinde comportamentul volatil al cursului EUR/RON în perioade de incertitudine economică (ex: pandemia COVID-19, războiul din Ucraina, inflație),
# este necesar un model care să permită variații ale varianței în timp. Modelul GARCH(1,1) este utilizat pentru a evidenția aceste fluctuații de volatilitate.

# Corectarea indexului temporal pentru a evita afișarea anului 1970 în grafic
aur_returns <- diff(eur$Price.Aur.EURO)
aur_returns <- na.omit(aur_returns)
aur_dates <- eur$Date[-1]  # eliminăm prima zi pierdută prin diferențiere
aur_returns_xts <- xts(aur_returns, order.by = aur_dates)

# Se specifică modelul GARCH(1,1) cu o componentă AR(1) pentru media condiționată.
# Alegerea modelului AR(1) este justificată prin existența unei autocorelări slabe în valorile diferențiate.
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(2, 0), include.mean = TRUE),
  distribution.model = "norm" )  # Se utilizează distribuția normală pentru reziduuri

# Estimarea modelului pe seria diferențiată
garch_model <- ugarchfit(spec = spec, data = aur_returns_xts)

# Afișarea rezultatelor modelului
show(garch_model)

# Interpretare:
# Coeficienții α1 (ARCH) și β1 (GARCH) sunt utilizați pentru a evalua dinamica volatilitații:
# - Un coeficient α1 semnificativ arată că șocurile recente (de exemplu, variații bruște ale cursului) au un efect imediat asupra volatilitații.
# - Un coeficient β1 ridicat sugerează că volatilitatea are memorie, adică efectele șocurilor persistă pe termen mai lung.
# - Dacă α1 + β1 ≈ 1, volatilitatea este persistentă, reflectând perioade de instabilitate continuă.

# Vizualizarea volatilitații estimate (deviația standard condiționată în timp)
volatility <- sigma(garch_model)

plot(volatility, type = "l", col = "blue", lwd = 2,
     main = "Volatilitatea estimată a cursului EUR/RON (GARCH(1,1))",
     ylab = expression(sigma[t]), xlab = "Timp")

#Modelul GARCH(1,1) aplicat asupra randamentelor EUR/RON, cu medie AR(2), 
#indică o volatilitate persistentă (α₁ + β₁ ≈ 1), semnalând că șocurile afectează cursul pe termen lung
#. Rezultatele testelor (Ljung-Box, ARCH-LM) arată reziduuri bine modelate, fără autocorelare sau heteroscedasticitate reziduală. 
#Totuși, testul de stabilitate (Nyblom) indică o ușoară instabilitate structurală, reflectând impactul unor evenimente externe (ex. pandemie, criză energetică).
#Din punct de vedere economic, volatilitatea ridicată confirmă că riscul valutar este semnificativ, mai ales în contexte geopolitice tensionate.

resid_std <- residuals(garch_model, standardize = TRUE)
resid_std
Box.test(resid_std, lag = 20, type = "Ljung-Box")
#Am fauct Testul Ljung-Box pe o luna , dupa ce am aplicat Garch . Am obtinut un coef de 28%
#Ceea ce ne indica faptul ca pentru o luna , lagurile nu sunt autocorelate


# PARTEA A 2 A ------------------------------------------------------------

#------SERII MULTIVARIATE-------

#Testul Johansen


mat <- cbind(ts_eur, ts_aur)

johansen_test <- ca.jo(mat, type = "trace", ecdet = "const", K = 2)
summary(johansen_test)

#Pe baza testului Johansen, a fost identificată o relație de cointegrare extrem de mica între cursul EUR/RON și prețul aurului, 
#confirmând existența unui echilibru de termen lung între cele două active. Deși seriile sunt nestationare individual, 
#cointegrarea sugerează o legătură structurală, în special în contexte de criză economică.Modelul indică faptul că prețul 
#aurului reacționează mai puternic la dezechilibre, acționând ca o variabilă de ajustare, în timp ce cursul valutar are o 
#inerție mai mare în fața șocurilor.

#Model VECM
# Estimarea modelului VECM (cu 1 relație de cointegrare – r = 1, după testul Johansen)
vecm <- cajorls(johansen_test, r = 1)
summary(vecm$rlm)

#Modelul VECM indică o relație de echilibru pe termen lung slabă între cursul EUR/RON și prețul aurului. Pe termen scurt, aurul reacționează semnificativ la dezechilibrele
#față de acest echilibru, ceea ce îl face un potențial activ de refugiu în fața instabilității valutare. În schimb, cursul valutar reacționează mai slab, dar este 
#influențat de propriile sale valori anterioare, ceea ce sugerează o inerție în ajustare.

# Extragem componentele pentru analiză și vizualizare
data_vecm <- vecm$rlm$model
ect <- data_vecm$ect1
resid_eur <- residuals(vecm$rlm)[, 1]
resid_aur <- residuals(vecm$rlm)[, 2]

ect_xts <- xts(ect, order.by = eur$Date[-c(1,2)])
df_ect <- data.frame(Data = index(ect_xts), ECT = coredata(ect_xts))
#ect1 are coeficient negativ și marginal semnificativ (p ≈ 0.069), ceea ce sugerează o corecție slabă a dezechilibrelor de la relația pe termen lung.
#Cursul EUR/RON se ajustează ușor când se abate de la echilibrul de termen lung, dar reacția este slabă.
#Cursul valutar este puternic autoregresiv — adică influențat de propriile variații anterioare.
#Prețul aurului nu pare să aibă un impact imediat (pe termen scurt) asupra cursului EUR/RON.
#ect1 este semnificativ (p ≈ 0.0035) și negativ ⇒ aurul reacționează la abaterile de la echilibru
#ΔEUR/RON (lag) și ΔAur (lag) nu sunt semnificative (p > 0.3 și p > 0.59).

# Vizualizare: componenta de corecție (ECT)
ggplot(df_ect, aes(x = Data, y = ECT)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Componenta de corecție ECT în VECM", x = "Timp", y = "ECT") +
  theme_minimal()

# Graficul evidențiază evoluția în timp a componentei de corecție ECT în cadrul modelului VECM.
# Se observă că până în jurul anului 2022, abaterile de la echilibru au fost moderate și oscilante în jurul valorii 0.
# După această perioadă, însă, apare o deviație negativă accentuată, care semnalează un dezechilibru semnificativ între cursul EUR/RON și prețul aurului.
# Această ruptură poate fi asociată cu șocuri economice externe, precum inflația globală, efectele persistente ale pandemiei, războiul din Ucraina
# sau anticiparea alegerilor prezidențiale, care au afectat piața valutară.
# În finalul perioadei analizate, cursul EUR/RON pare să se îndepărteze tot mai mult de traiectoria dictată de evoluția aurului, 
# ceea ce justifică aplicarea modelelor de volatilitate și analiză de risc în pașii următori.


#IRF
vecm_var <- vec2var(johansen_test, r = 1)

# Generăm funcția de răspuns la impuls (IRF)
irf_model <- irf(
  vecm_var,
  impulse = "ts_aur",        # șoc aplicat aurului
  response = "ts_eur",       # răspunsul observat la curs
  n.ahead = 20,                  # orizont de 20 de perioade
  boot = TRUE                    # bootstrap pentru intervale de încredere
)

# Vizualizare IRF
plot(irf_model)

# Graficul IRF arată cum reacționează cursul EUR/RON la un șoc pozitiv în prețul aurului exprimat în euro.
# Linia neagră reprezintă efectul estimat, iar liniile roșii definesc intervalul de încredere bootstrap la 95%.
# Se observă că un șoc asupra aurului determină o creștere ușoară, dar semnificativă statistic, a cursului EUR/RON pe termen scurt și mediu.
# Acest lucru înseamnă că o creștere bruscă a prețului aurului (de ex. cauzată de o criză globală) duce la deprecierea leului față de euro.
# Efectul persistă pe parcursul celor 20 de perioade analizate, sugerând că aurul are un impact economic durabil asupra cursului valutar.
# Rezultatul este logic din punct de vedere economic: aurul, ca activ de refugiu, se apreciază în perioade de instabilitate,
# ceea ce este însoțit frecvent de deprecierea monedelor emergente, inclusiv RON.

eur_diff <- na.omit(diff(eur$Pret.EURO.LEI))
aur_diff <- na.omit(diff(eur$Price.Aur.EURO))

# Aliniem lungimea seriilor
min_len <- min(length(eur_diff), length(aur_diff))
eur_diff <- eur_diff[1:min_len]
aur_diff <- aur_diff[1:min_len]

# Combinăm în matrice și creăm model VAR
data_var <- cbind(eur_diff, aur_diff)
colnames(data_var) <- c("eur", "aur")

# Selectăm automat lagul optim (poți ajusta dacă vrei)
lag_select <- VARselect(data_var, lag.max = 10, type = "const")
opt_lag <- lag_select$selection["AIC(n)"]
opt_lag
# Estimăm modelul VAR
model_var <- VAR(data_var, p = opt_lag, type = "const")
model_var
# Testăm dacă aur influențează cursul
granger1 <- causality(model_var, cause = "aur")
print(granger1$Granger)

# Testăm dacă cursul influențează aurul
granger2 <- causality(model_var, cause = "eur")
print(granger2$Granger)

# Testul 1: aur → EUR/RON
# H0: Prețul aurului NU cauzează cursul EUR/RON
# Rezultat: p-value = 0.06689 → NU respingem H0 (la pragul de 5%)
#
# Testul 2: EUR/RON → aur
# H0: Cursul EUR/RON NU cauzează prețul aurului
# Rezultat: p-value = 0.0925 → NU respingem H0 (la pragul de 5%)

# Concluzie:
# Niciuna dintre serii nu cauzează semnificativ cealaltă în sensul Granger, la un nivel de încredere de 95%.
# Totuși, ambele p-value sunt aproape de 0.05, sugerând o potențială relație slabă de influență pe termen scurt,
# dar nu una consistentă din punct de vedere statistic.
#
# Acest rezultat este în contrast cu analiza VECM, care a evidențiat o relație structurală pe termen lung slaba , dar una pe termen scurt in conditii de dezechilibru.
# Astfel, putem concluziona că:
# - Pe termen **scurt**, relația dintre aur și curs este puternica (doar in momentele de dezechilibru).
# - Pe termen **lung**, conform cointegrare + VECM, există o legătură economică slaba între cele două.

#Functie de raspuns la impuls

irf_result <- irf(model_var, impulse = "aur", response = "eur", n.ahead = 30, boot = TRUE)
plot(irf_result, main = "Răspunsul cursului EUR/RON la un impuls în prețul aurului")
# Graficul IRF arată reacția cursului EUR/RON la un șoc pozitiv în prețul aurului, pe un orizont de 30 de perioade.
# Se observă un răspuns pozitiv imediat în primele 2–3 perioade, urmat de o corecție ușoară și revenire spre zero.
# Acest efect scurt și slab semnificativ sugerează că impactul prețului aurului asupra cursului EUR/RON este limitat pe termen scurt.
#
# Linia neagră reprezintă valoarea estimată a răspunsului, iar liniile roșii sunt intervalele de încredere de 95% (bootstrap).
# Deoarece răspunsul iese ușor din banda de incertitudine doar în primele perioade, putem concluziona că șocurile în prețul aurului
# au un efect tranzitoriu și moderat asupra evoluției cursului valutar.
#
# Acest rezultat este în linie cu analiza Granger, care nu a identificat o relație cauzală semnificativă între cele două variabile pe termen scurt,
# dar și cu modelul VECM, care a confirmat o relație structurală de lungă durată.


fevd_result <- fevd(model_var, n.ahead = 30)
fevd_table <- data.frame(
  Orizont = c(1, 10, 30),
  `EUR explicat de EUR` = round(c(fevd_result$eur[1,1], fevd_result$eur[10,1], fevd_result$eur[30,1]) * 100, 2),
  `EUR explicat de Aur` = round(c(fevd_result$eur[1,2], fevd_result$eur[10,2], fevd_result$eur[30,2]) * 100, 2),
  `Aur explicat de EUR` = round(c(fevd_result$aur[1,1], fevd_result$aur[10,1], fevd_result$aur[30,1]) * 100, 2),
  `Aur explicat de Aur` = round(c(fevd_result$aur[1,2], fevd_result$aur[10,2], fevd_result$aur[30,2]) * 100, 2)
)
print(fevd_table)

# Interpretare – Analiza de descompunere a varianței (Forecast Error Variance Decomposition - FEVD):
# 
# Această analiză evidențiază cât de mult din variația unei variabile (EUR sau aur) este explicată de propriile șocuri
# și de cele ale celeilalte variabile, pe diferite orizonturi de timp.
# 
# Rezultate cheie:
# - Pentru EUR/RON:
#   - Pe termen scurt (orizont 1): variația este explicată 100% de propriile șocuri.
#   - Pe termen mediu și lung (orizont 10 și 30): contribuția aurului rămâne foarte scăzută (~0.27%), ceea ce arată că
#     evoluția cursului este determinată în principal de factorii săi interni (politici monetare, inflație, economie locală).
# 
# - Pentru aur:
#   - Pe termen scurt (orizont 1): aproape 100% din variație este explicată de șocuri proprii.
#   - Pe termen lung (orizont 30): influența EUR/RON rămâne foarte slabă (sub 0.4%), confirmând că prețul aurului este
#     dominat de factori internaționali și nu de cursul valutar local.
# 
# Concluzie:
# - Atât cursul EUR/RON, cât și prețul aurului sunt variabile autoregresive, explicate aproape complet de propriile dinamici.
# - Legătura cauzală și influența reciprocă sunt minime, ceea ce întărește concluziile anterioare privind o relație slabă
#   în termeni de prognoză a varianței, în ciuda cointegrației identificate pe termen lung.

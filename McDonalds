#McDonalds activează in ndustria alimentară – sectorul de servicii alimentare rapide (fast-food)
#De asemenea , concurenții directi  ai lui Mc sunt KFC , Wendys , Burger King , insă concurenta cea mai puternică
#concurenta pe care o are McDonalds , pe piata SP500 este Starbucks , Chipotle , Yum! Brands care detine francizele KFC ,Taco Bell si Pizza Hut in U.S
# si Restaurant Brands International care detine francizele Burger King ,Popeyes etc. in U.S
#Am ales sa compar McDonalds cu Chipotle deoarece este singura companie independenta care nu apartine unui lant de branduri ca restu
#având totodată un pret destul de ridicat pe piata , fiind totodata destinata tineretului , viitorul acesteia fiind unul destul de promitator
#Forma de concurenta este monopolistica pe indicele de piata SP500 , fiind oferite produse similare , dar care au diferentieri importante
#pe acest segment de piata ,cea alimentar , este oligopola , fiind doar cateva companii destinate fast-food gigantie
#Intrarea pe genul acesta de piata este foarte dificilă , mai ales din cauza companiilor care detin multe lanturi de fast-food populare
# Astfel că preturile si conditiile pot fi puse intre ei ,se poate crea astfel cartel
#Preturile sunt impuse atât de costurile produselor care pot determina pretul produselor , cota de piata , cât si actiunile celorlalte companii
# Însa putem observa preturile similare pe care le au Mc si Burger King 
#Companiile isi atrag clientii prin reclame si scheme de marketing , promotii cum ar fii combouri noi puse pe piata de catre Mc , reduceri din aplicații
#Produse noi care atrag publicul/campanii , ca de exemplu cea de acum pe care o are McDonalds cu Minecraft , unde la achizitionarea unui meniu special in valoare de
# 30 de lei primesti o figurina minecraft + un skin McDonalds in joc . O ideea foarte ingenioasa din partea celor de la Mc pentru a atrage cat mai mult public
#De asemenea o alta metoda ingenioasa de a atrage cumparatorii sa achizitioneze cat mai multa mancare sunt punctele pe care le poti strange in aplicatia Mc .
#Cu cat achizitionezi mai multe produse , cu atat primesti mai multe puncte , puncte cu care poti sa achizitionezi ulterior produse gratis
# Acest sistem este intlanit si la alte fast-food-uri cum ar fi Taco Bell , Burger King.

setwd("E:/Folder nou")
pt<-read.csv(file="micro-excel2.csv" , header=T , sep=";")
pr<-read.csv(file="proiectmicro1.csv" , header=T , sep=";")

ts.plot(pt[,c(2,6)] , main="Fig.30 Capitalul pe piata bursiera a celor 2 companii (Bill)" , xlab="trimestre" , ylab="Valoare" , col=c("darkred","lightgreen"))
#Putem observa faptul că  capitatul pe piata a lui Mc este mult peste cel a lui Chipotle , cu o crestere exponentiala
#insa Chipotle pare sa aibe o scadere a capitalului pe piata bursiera. Aceste cauze pot fi din cauza schimbarii CFO , cat
#si anunutul pe care l-a facut compania cu privire la vanzarile pe 2025

ts.plot(pt[,c(5,9)] , main="Fig.31 Profitul Brut a celor 2 companii " , xlab="trimestre" , ylab="Valoare" , col=c("darkred","lightgreen"))
#Putem observa ca ambele au inregistrat cresteri ale profitului 
#Chipotle a intampinat profituri considerabile , cu o crestere de la inceput de an pana in prezent de 76% 
#Însa Mc nu a avut parte de aceleasi rezultate , dar totusi rezultatul a fost unul destul de bun ,de 17%
RtcCh1<-(pt$ProfitBrut_Chipotle[length(pt$ProfitBrut_Chipotle)]-pt$ProfitBrut_Chipotle[1])/pt$ProfitBrut_Chipotle[1]*100
RtcMc1<-(pt$ProfistBrut_Mc[length(pt$ProfistBrut_Mc)]-pt$ProfistBrut_Mc)/pt$ProfistBrut_Mc[1]*100

ts.plot(pt[,c(3,7)] , main="Fig.32 Venituri totale ale celor 2 companii " , xlab="trimestre" , ylab="Valoare" , col=c("darkred","lightgreen"))
#Acelasi lucru il putem spune si despre profituri , unde profitul pentru Chipotle pare sa aibe o crestere mai mare
ts.plot(pt[,c(4,8)] , main="Fig.33 Cheltuieli totale ale celor 2 companii " , xlab="trimestre" , ylab="Valoare" , col=c("darkred","lightgreen"))
#Putem observa faptul ca Chipotle a avut o cheltuieli mult mai mari decat Mc
#Motivele pentru acest lucru sunt:
#Creșterea costurilor ingredientelor esențiale
#Prețurile unor ingrediente-cheie, precum avocado, carne de vită, brânză și smântână, 
#au crescut considerabil. De exemplu, exporturile de avocado din Mexic au fost temporar 
#suspendate din motive de securitate, ceea ce a redus oferta și a dus la majorarea prețurilor. 
#În aprilie 2024, Chipotle a crescut prețurile meniurilor cu 6-7% în California, 
#ca răspuns la creșterea salariului minim impusă de legislația locală. Ulterior, în decembrie 2024, compania a aplicat o majorare 
#de 2% la nivel național pentru a compensa inflația și costurile operaționale crescute.
#Chipotle a investit în tehnologii precum "Autocado", un aparat automatizat pentru 
#procesarea avocadoului, cu scopul de a reduce timpul și costurile asociate preparării guacamolei. Aceste investiții au generat 
#cheltuieli suplimentare pe termen scurt, dar vizează îmbunătățirea eficienței pe termen lung.
#De asemenea , cheltuielile pentru Mc sunt mai mari , insa este un lucru normal daca este
#sa raportam da VT si capitalul companiei Mc , care este pe piata de zeci de ani 
#Insa putem observa ca la un moment dar au avut o scadere de cheltuieli , unde dupa a crescut din nou
RtcCh<-(pt$CT_Chipotle[length(pt$CT_Chipotle)]-pt$CT_Chipotle[1])/pt$CT_Chipotle[1]*100
#Putem observa cresterea uriasa a CT pentru Chipotle de la inceputul anului pana la sfarsit , o crestere de 42%
RtcMc<-(pt$CP_Mc[length(pt$CP_Mc)]-pt$CP_Mc[1])/pt$CP_Mc[1]*100
#Pe cand McDonalds a avut o crestere doar de 5,3%
#Putem concluziona faptul ca , 
#Putem concluziona faptul ca Chipotle dispune , de datorii destul de mari , motiv pentru care exista aceasta scadere a capitalului pe piata bursiera
#McDonalds a avut o crestere pe aceasta perioada de timp

NetMc<-pt$VT_Mc-pt$CP_Mc
NetMc
NetCh<-pt$VT_Chipotle-pt$CT_Chipotle
NetCh
pt<-cbind(pt,NetCh,NetMc)
ts.plot(pt[,c(14,15)] , main="Fig.34 Cheltuieli totale ale celor 2 companii " , xlab="trimestre" , ylab="Valoare" , col=c("darkred","lightgreen"))

ROAMc<-NetMc/pt$ActivTot_Mc
ROAMc
ROACh<-NetCh/pt$ActiveTot_Chipotle
ROACh
#Putem observa ca rentabilitatea activelor este mai mare pentru Chipotle pe perioada acestui an
#Ceea ce inseamna ca Chipotle a reusit sa-si gestioneze mai bine activele decat Mc
CPMc<-pt$ActivTot_Mc-pt$Datorii_Mc
CPCh<-pt$ActiveTot_Chipotle-pt$Datorii_Ch

ROEMc<-NetMc/CPMc
ROEMc
ROECh<-NetCh/CPCh
ROECh
#Putem observa faptul că Chipotle incepe cu un randament al investitiilor de 36,6% si termina anul cu un randament de 96,3% 
#ceea ce reprezinta o performanta uriasa
#Mc reuseste sa tina acest randament oarecum constat , randamentul maxim fiind ultimul trimestru
#Aceasta crestere in prezent a randamentului pentru Chipotle , se datoreaza atat cresterii veniturilor , cat si prin deschiderea
#a 99 de restaurante

#Pentru piata pe care activeaza Mc , am luat sectorul Consumar Cyclical , Restaurant ,unde  concurentii sunt
# Starbucks , Chipotle , Yum!Brands , Darden Restaurants , Dominos Pizza
#Piata are un Market Cap total de 576,234 (bill) cu un nr de 58 de companii. Companiile cu valoarea cea  mai mare 
#pe piata bursiera sunt McDonalds , StarBucks si Chipotle



pr$Pret_S.P.500<- as.numeric(gsub(",", "", pr$Pret_S.P.500))
str(pr)

summary(pr$Pret_McDonald.s)

par(mfrow=c(2,2))
hist(pr$Pret_McDonald.s , main="Fig.2 Histograma preturilor Mc pe per.04.apr.2024-04.arp.2025 ",type="h",col="darkred", xlab="Preturi")
plot(pr$Pret_McDonald.s , main="Fig.1 Prețul acțiunilor McDonalds pe 04.apr.2024-04.arp.2025" ,type="h", xlab="Zile" , ylab="Pret" , col="darkred")
amp_Mc<-max(pr$Pret_McDonald.s)-min(pr$Pret_McDonald.s)
pr[which.max(pr$Pret_McDonald.s),]
#Putem observa ca de la începutul lui aprilie până pe 04.apr.2025 preturile acțiunii McDonalds au avut o creștere considerabilă
#avand o crestere maxima de 75.47 dolari, la data de  7 martie 2025 (linia 231) ,ajungand la pretul de 321,29 dolari. Acest lucru se datorează unui Q4 foarte bun pe anul 2024
#cât și alți factori cum ar fi creșterea industriei de fastfood și amânarea de către Trump a noii legi care implică un nou nivel ridicat
#de taxe pe spațiul American (Investopedia)
pr[which.min(pr$Pret_McDonald.s),]
#De asemenea putem observa faptul că pretul minim este atins la pretul de 245,82 de dolari , la dat de 9 iulie 2024 
#urmata mai apoi de crestere brusc , de la ziua 65 pana apropae de ziua 140 . Scaderea a avut loc  din cauza declaratiilor directorului financiar
#cu privire la faptul ca din cauza situatiei financiare a oamenilor , acestia prefere sa manance acasa . De asemenea in acea perioada compania a intampinat 
#un numar neasteptat de vanzari . Pentru a trece peste , s-au introdus oferte noi si ieftine in valoare de 5 dolari. 
#Acest lucru datoreaza cresterea brusca din grafic
#Se obseva ca frecventa cea mai mare de preturi ale actiunii au fost intre 290 si 300 de dolari 
sd_Mc<-sd(pr$Pret_McDonald.s)
#Putem observa faptul ca abaterea este destul de mica fata de  medie ( 290,1 )

summary(pr$Pret_S.P.500)

par(mfrow=c(2,2))
hist(pr$Pret_S.P.500 , main="Fig.3 Histograma preturilor S.P.500 pe per.04.apr.2024-04.arp.2025 ",type="h",col="darkblue", xlab="Preturi")
plot(pr$Pret_S.P.500 , main="Fig.4 Prețul acțiunilor S.P.500 pe 04.apr.2024-04.arp.2025" ,type="h", xlab="Zile" , ylab="Pret" , col="darkblue")
#Putem observa faptul ca preturile indicelui SP500 au avut o crestere de la inceputul lunii aprilie 2024 pana la sfarsitul perioadei , 04.04.2025 , maximul fiind la pretul de 6.144 de dolari
#Putem observa faptul ca frecventa celui mai mare pret este intre 6000 si 6100 

pr[which.max(pr$Pret_S.P.500),]
pr[which.min(pr$Pret_S.P.500),]
#cel mai mic pret s-a inregistrat pe 19 aprilie 20204 , acesta fiind 4967,23 .Acest lucru se datoreaza declinului sectorului tehnologoc ,
#pt companii cum ar fi Nvidia si Super Micro Computer. 
#cel mai mare pret al indicelui de piata S.P500 a fost pe data de 19 februarie 2025 , la pretul de 6144,15 dolari . Acest lucru a avut loc din cauza cresterii actiunilor Tesla
#lucru datorat lui Donald Trump care a facut in asa fel incat sa faciliteze cresterea companiei Tesla
#Cresterea brusca pe parcursul lunilor se datoreaza : politici comerciale mai permisive , cresterea tuturor companiilor care apartin de acest indice si increderea
#investitorilor in sistemul de munca al SUA
amp_SP500<-max(pr$Pret_S.P.500)-min(pr$Pret_S.P.500)
#Se observa o diferenta de 1176,92 de dolari intre pretul minim si cel maxim . Acest lucru ne indica o crestere constanta si foarte mare a preturilor indicelui SP500
sd_SP500<-sd(pr$Pret_S.P.500)
#Putem observa faptul ca abaterea este destul de mica fata de  medie ( 5650 )

summary(pr$Pret_Chipotle)

par(mfrow=c(2,2))
hist(pr$Pret_Chipotle , main="Fig.5 Histograma preturilor Chipotle pe per.04.apr.2024-04.arp.2025 ",type="h",col="lightgreen", xlab="Preturi")
plot(pr$Pret_Chipotle , main="Fig.6 Prețul acțiunilor Chipotle pe 04.apr.2024-04.arp.2025" ,type="h", xlab="Zile" , ylab="Pret" , col="lightgreen")
pr[which.max(pr$Pret_Chipotle),]
pr[which.min(pr$Pret_Chipotle),]
#Există două maxime evidente (în jurul zilelor 90 și 200), unde prețurile ating petse 60 de dolari , maximul atins fiind de 68 in ziua 2000 ( 22 ian 2025).
#De asemenea graficul pleaca din prima zi cu punctul cel mai mic , acesta fiind de 47,29 dolari ( 5 apr 2024)
#Perioadele de scădere accentuată sunt clare între zilele 100–150 și din nou spre final (după ziua 220), unde prețurile revin spre 54–56 USD.
#Variabilitatea este evidentă, dar nu extremă – Chipotle pare să fi trecut prin cicluri de optimism și corecție.
#Prețurile s-au concentrat în jurul valorii de 58–60 dolari, fiind cele mai frecvente în perioada analizată.
#Există și apariții semnificative în intervalele 55–58 USD și 60–63 USD, ceea ce sugerează o relativă stabilitate a prețurilor în jurul acestor valori.
#Prețurile sub 50 USD și peste 65 USD sunt rar întâlnite, indicând extreme (minime și maxime).
amp_Chipotle<-max(pr$Pret_Chipotle)-min(pr$Pret_Chipotle) # diferenta de pret , de maximul atins si minimul atins este de 7,1%
# ceea ce reprezinta destul de mult avand in vedere pretul relativ mic pe piata al companiei , deci putem spune faptul ca in acest moment
# compania se confrunta cu niste probleme majore
sd_Chipotle<-sd(pr$Pret_Chipotle)
# deviatia standard este mai mica decat pretul mediu

library(moments)
library(corrplot)
sk_Mc<-skewness(pr$Pret_McDonald.s)
k_Mc<-kurtosis(pr$Pret_McDonald.s)
k_SP<-kurtosis(pr$Pret_S.P.500)
sk_SP<-skewness(pr$Pret_S.P.500)
k_Ch<-kurtosis(pr$Pret_Chipotle)
sk_Ch<-skewness(pr$Pret_Chipotle)

#Se observa o asimetrie  la stanga din partea la toti cei 3 indicii Mc,Ch si SP500(-0,33 ,-0,19si -0,29) , acestia fiind totoadata si platicurtice , ambii coef k<3

mat_cor<-cor(pr[2:4])
corrplot(mat_cor,method = c("number"),type = c("lower"),title="Matrice corelatie",bg="lightpink")
# Din urma graficului de corelatie putem trade urmatoarele concluzii : 1. McDonalds are un impact aproape dublu pe piata SP500, fata de Chipotle
#2. Fiind o chestie destul de logica si evidenta , corelatia dintre Mc si Chipotle este 0,1 , foarte mica , lucru datorat competitiei celor 2 companii , care fac parte totodata din
# acelasi sector de piata (FastFood)
cv_Mc<-sd(pr$Pret_McDonald.s)/mean(pr$Pret_McDonald.s)
cv_SP<-sd(pr$Pret_S.P.500)/mean(pr$Pret_S.P.500)
cv_Ch<-sd(pr$Pret_Chipotle)/mean(pr$Pret_Chipotle)
#Se observa faptul ca nu exista outlieri in toate cele 3 grafice boxplot , deci nu au fost valori extreme
# Se observa de asemenea ca Mc a avut mai multe preturi mari pe actiuni decat media , Chipotle se afla in exhilibru 
#Iar pretul indicelui pe piata S.P.500 are bara care reprezinta media preturilor putin mai la stanga semn ca a avut putin mai multe
# preturi mai mici decat media , de asemenea a avut variatia mult mai mare de preturi mici decat mari
#Seriile de preturi de la toate cele 3 companii sunt omogene ( 5,42% , 6,66% , 7,78%) , cu medii reprezentative
#cele mai imprastiate preturi putem spune ca le are compania ( comparativ cu celelalte 2)

matrice<-matrix(nrow=3 , ncol=5 ,dimnames =list(c("McDonalds","S.P.500","Chipotle"),c("Amplitudinea","Deviatia standard","Coef de aplatizare","Coef de boltire (k)","Coeficientul de variatie")))
matrice[,1]<-c(amp_Mc,amp_SP500,amp_Chipotle)
matrice[,2]<-c(sd_Mc,sd_SP500,sd_Chipotle)
matrice[,3]<-c(sk_Mc,sk_SP,sk_Ch)
matrice[,4]<-c(k_Mc,k_SP,k_Ch)
matrice[,5]<-c(cv_Mc,cv_SP,cv_Ch)
View(matrice)

par(mfrow=c(3,1))
boxplot(pr$Pret_McDonald.s,col="darkred",main="Fig.7 Boxplot Pret McDonalds",horizontal = T)
boxplot(pr$Pret_S.P.500,col="darkblue",main="Fig.8 Boxplot Pret indice de piata S.P.500",horizontal = T)
boxplot(pr$Pret_Chipotle,col="lightgreen",main="Fig.9 Boxplot Pret Chipotle",horizontal = T)

stats1<-boxplot.stats(pr$Pret_McDonald.s)$out
stats2<-boxplot.stats(pr$Pret_Chipotle)$out
stats3<-boxplot.stats(pr$Pret_S.P.500)$out


#Pentru a evidentia interpretarea facuta la boxploturi care se bazeaza pe nr de preturi mai mari decat media
# am realizat 3 grafice care evidentiaza acest lucru
val_med_Mc<-which(pr$Pret_McDonald.s>mean(pr$Pret_McDonald.s),)
val_med_SP500<-which(pr$Pret_S.P.500>mean(pr$Pret_S.P.500),)

plot(val_med_Mc, col="darkred", main="Fig.10 Preturile actiunii McDonalds mai mari decat medie" , type="h", xlab="zile",ylab="pret actiuni")
# putem observa faptul ca avem 150 cu preturi mai mari decat media si 101 unde sunt mai mici
plot(val_med_SP500, col="darkblue", main="Fig.11 Preturile indicelui de piata S.P.500 mai mari decat medie" , type="h", xlab="zile",ylab="pret actiuni")
#Un lucru foarte interesant este ca in boxplot putem observa ca linia din cutie ( media ) tinde sa fie mai la stanga
# dar aici putem observa ca sunt 126 de zile cu preturi mai mari decat media , adica cu o zi mai mult fata de preturile unde
#media a fost mai mica , adica foarte la limita
val_med_Ch<-which(pr$Pret_Chipotle>mean(pr$Pret_Chipotle),)
plot(val_med_Ch, col="lightgreen", main="Fig.12 Preturile actiunii Chipotle mai mari decat medie" , type="h", xlab="zile",ylab="pret actiuni")
#Desi media parea sa fie in punct de echilibru , se observa ca sunt 134 de zile cu preturi mai mari decat media

ts.plot(pr[,c(2:4)] , main="Fig.13 Preturi McDonalds , S.P.500 si Chipotle", col=c("darkred","darkblue","lightgreen") )
#In acest grafic care indica preturile pentru cele doua companii si indicele de piata S.P500 , se observa ca McDonalds este peste Chipotle

McDonalds_norm<-apply(as.matrix(pr[,2]),MARGIN=2,FUN=function(x){return((x-mean(x))/sd(x)) } )
S.P.500_norm<-apply(as.matrix(pr[,3]),MARGIN=2,FUN=function(x){return((x-mean(x))/sd(x)) } )
Chipotle_norm<-apply(as.matrix(pr[,4]),MARGIN=2,FUN=function(x){return((x-mean(x))/sd(x)) } )
pr<-cbind(pr,McDonalds_norm,S.P.500_norm,Chipotle_norm)
#Prin standardizarea datelor observam valoriile peste medie , cam acelasi lucru pe care l am facut la boxplot
#Doar ca in acest caz in care avem de comparat 2 companii cu preturi semnificativ diferite pe piata , ne ajuta sa
#vedem care a avut companie a avut o crestere mai buna pe aceasta perioada de timp
par(mfrow=c(3,3))
plot(McDonalds_norm, col="darkred", main="Fig.14 Preturile actiunii McDonalds standardizate" , type="h", xlab="zile",ylab="pret actiuni")
plot(S.P.500_norm, col="darkblue", main="Fig.15 Preturile indicelui de piata S.P.500 standardizate" , type="h", xlab="zile",ylab="pret actiuni")
plot(Chipotle_norm, col="lightgreen", main="Fig.16 Preturile actiunii Chipotle standardizate" , type="h", xlab="zile",ylab="pret actiuni")
hist(McDonalds_norm , main="Fig.17 Histograma preturilor McDonalds standardizate ",type="h",col="darkred", xlab="Preturi")
hist(S.P.500_norm , main="Fig.18 Histograma preturilor S.P.500 standardizate ",type="h",col="darkblue", xlab="Preturi")
hist(Chipotle_norm , main="Fig.19 Histograma preturilor Chipotle standardizate",type="h",col="lightgreen", xlab="Preturi")
# in graficele plot putem obseva faptul ca scaderile sunt unele mai intense decat cresterile , desi sunt mai mici in numar
#observam de asemenea un numar mare de preturi ale SP500 care au indicele de standardizare intre 1-1,5 ceea ce arata ca sunt multe preturi care au avut
#cresteri foarte mari , un lucru bun am putea spune
#acelasi lucru il putem spune si la McDonalds 
# la Chipotle cele mai mari frecventa au fost intre -0,5-0 si 0-0,5 
#la Mc si indicele de piata SP500 obs ca inceputul perioadei porneste cu o scadere , urmata mai apoi de o crestere , insa la final de an SP500 are o scadere brusca
#la Chipotle insa situatia este destul de complicata , deoarecele avem cresteri urmate de coborari , formanduse un ciclu

Rt_Mc<- (pr$Pret_McDonald.s[-1] / pr$Pret_McDonald.s[-length(pr$Pret_McDonald.s)]) - 1
Rt_S.P.500<- (pr$Pret_S.P.500[-1] / pr$Pret_S.P.500[-length(pr$Pret_S.P.500)]) - 1
Rt_Ch<- (pr$Pret_Chipotle[-1] / pr$Pret_Chipotle[-length(pr$Pret_Chipotle)]) - 1
Rent<-cbind(Rt_Mc,Rt_S.P.500,Rt_Ch)

par(mfrow=c(3,3))
plot(Rt_Mc, col="darkred", main="Fig.20 Rentabilitateaactiunii McDonalds " , type="h", xlab="zile",ylab="pret actiuni")
plot(Rt_S.P.500, col="darkblue", main="Fig.21 Rentabilitateaindicelui de piata S.P.500 " , type="h", xlab="zile",ylab="pret actiuni")
plot(Rt_Ch, col="lightgreen", main="Fig.22 Rentabilitatea actiunii Chipotle " , type="h", xlab="zile",ylab="pret actiuni")
hist(Rt_Mc , main="Fig.23 Histograma rentabilitatii McDonalds ",type="h",col="darkred", xlab="Preturi")
hist(Rt_S.P.500 , main="Fig.24 Histograma rentabilitatii S.P.500 ",type="h",col="darkblue", xlab="Preturi")
hist(Rt_Ch, main="Fig.25 Histograma rentabilitatii Chipotle ",type="h",col="lightgreen", xlab="Preturi")
#Cea mai mare rentabilitate s-a inregistrat la Chipotle , cea mai mica fiind a lui Mc -0,6 , insa Chipotle are mai multe rentabilitati slabe la -0,5
#decat Mc la -0,6
#Singurul lucru pe care il putem spune despre ploturi este faptul ca Chipotle are variatii de rentabilitate extrem de mari , fata de restul de 2 indici
val_rent_MC<-which(Rt_Mc>0,)
val_rent_MC
#Putem observa ca rentabilitatea a fost una pozitiva
val_rent_SP500<-which(Rt_S.P.500>0,)
val_rent_SP500
#Acelasi lucru si la indicele SP500 , unde a fost mai mare decat McDonalds
val_rent_Ch<-which(Rt_Ch>0,)
val_rent_Ch
#Insa putem observa ca rentabilitatea pentru Chipotle a fost usor negativa , acest lucru putand fii observat oarecum si in histograma
ts.plot(Rent[,c(1:3)] , main="Fig.26 Rentabilitati McDonalds , S.P.500 si Chipotle", col=c("darkred","darkblue","lightgreen") )
# Si aici se observa variatia mare a Chipotle

par(mfrow=c(3,1))
boxplot(val_rent_MC,col="darkred",main="Fig.27 Boxplot Rentabilitate McDonalds",horizontal = T)
boxplot(val_rent_SP500,col="darkblue",main="Fig.28 Boxplot Rentabilitate indice de piata S.P.500",horizontal = T)
boxplot(val_rent_Ch,col="lightgreen",main="Fig.29 Boxplot Rentabilitate Chipotle",horizontal = T)
#Observam faptul că nu avem outlieri 
#Variabilitatea rentabilitătilor este peste tot cama aceasi
#Diferenta dintre rentabilitătile peste medie si cele care nu sunt este mica
mat_cor_rent<-cor(Rent)
corrplot(mat_cor_rent,method = c("number"),type = c("lower"),title="Fig.35 Matrice corelatie a rentabilitatilor",bg="lightpink")
#Putem observa faptul ca SP500 are oarecum o contributie in cresterea rentabilitatii McDonald , fiind un coeficient de 0,25 
#Acesta nu e mare , dar are o mica importanta in stabilitatea firmei
#Pt Chipotle insa este chiar mai mica , asta pentru ca probabil nu se afla la nivelul lui Mc , unde SP500 si McDonalds depind  una de
#cealalta intr-o anumita masura

# Problema Economica Propusa ----------------------------------------------

#Lucru important din punctul meu de vedere este faptuc că , oare justifică investiția în McDonald’s în detrimentul unui competitor din S&P 500 (ex: Chipotle),
#analizând rentabilitatea ajustată la risc? Compar rentabilitatea celor două companii ajustată la volatilitate , ca să văd 
#care ar fi o alegere mai bună din perspectiva unui investitor rațional.

#În problema voi folosii randamentele anuale pentru Mc si Chipotle si deviatia standard(volatilitatea) pentru
# a ajusta randamentul la risc , iar apoi le compar
Rand_MC<-(pr$Pret_McDonald.s[length(pr$Pret_McDonald.s)]-pr$Pret_McDonald.s[1])/pr$Pret_McDonald.s[1]*100
Rand_Ch<-(pr$Pret_Chipotle[length(pr$Pret_Chipotle)]-pr$Pret_Chipotle[1])/pr$Pret_Chipotle[1]*100

randamente_log1 <- diff(log(pr$Pret_McDonald.s))
sd_rand_Mc<-sd(randamente_log1)
randamente_log2 <- diff(log(pr$Pret_Chipotle))
sd_rand_Ch<-sd(randamente_log2)

RrMc<-Rand_MC/sd_rand_Mc
RrCh<-Rand_Ch/sd_rand_Ch
RrMc
RrCh
#Se observa in final ca ar fi fost mai rentabila investitia in Chipotle raportat la risc
#Acest lucru se observa in urma analizei , Mc fiind deja o campanie foarte mare , cu un nivel foarte greu de crestere a
#preturilor de actiune , pe cand riscul unui de a declansa o scădere a pretului pe piata este relativ mare
#Acest lucru reiese si din P/E Ratio care este x27 (investitorul isi recupereaza banii in 27 de ani in cazul in care pretul stagneaza )
#Este un procent foarte mare care ne sugereaza ca o investitie nu ar fi potrivita

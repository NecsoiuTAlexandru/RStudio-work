#aceste data sunt realizate pe anul 2023
library(cluster)
library(factoextra)
library(FactoMineR)
library(NbClust)
library(corrplot)
library(dplyr)
library(here)
library(ggrepel)
library(moments)

#install.packages("ggrepel")
#install.packages("FactoMineR") 
#install.packages("cluster")
#install.packages("factoextra") 

actint<-read.csv(file="activ internet.csv", header=T , sep=";")
intr<-read.csv(file="intreprinderi.csv" , header=T , sep=";")
list.files()
date_finale <- merge(actint, intr, by = "GEO..Labels.")

set.seed(123)

summary(actint)
#pers.active.sm – Persoane active pe rețele sociale (%),Medie: 75.25%, cu variații între 47.82% și 92.13%
#pers.inf.la.b.s – Persoane care se informează online înainte de cumpărături ,Medie: 79.77%
#fol.apl.mes.dir – Folosirea aplicațiilor de mesagerie directă (WhatsApp, Messenger etc.),Medie: 85.22%
#sec.datpers.sm..mgonl – Utilizatori preocupați de securitatea datelor personale online,Medie: 41.33%
#Persoane care interacționează cu conținut sponsorizat pe social media,Medie: 26.44%
#pers.vand.lucr.serv – Persoane care vând online bunuri/servicii,#Medie: 22.07%
#pers.ach.in.ult.3.l – Persoane care au făcut achiziții online în ultimele 3 luni ,Medie: 61.94%
#Se observa indici a caror medii depasesc 60-70% ceea ce indica multe tari cu un nivel ridicat al acelui indice
#De asemenea insa avem medii scazute pe indici cum ar fi informarea cu privire la bunuri si servicii , vinderea de produse
#informarea cu privire la continutul de pe internet. Lucru ca ne indica faptul ca desi majoritatea tarilor sunt active in mediul social , sunt slab
#educate cu privire la mediul virtual . Se observa diferente mari intre minimul si maximul indicilor , amplitudinea fiind in unele cazuri
# si de 65
#Se observa faptul ca media > mediana in cazul indicilor de consum si activitate , insa mai mica pentru indicii
#legati de informare si educatie digitala
act_sd<-sd(actint$pers.active.sm)
inf_sd<-sd(actint$pers.inf.la.b.s)
vand_sd<-sd(actint$pers.vand.lucr.serv)
cont_sd<-sd(actint$pers.inf.la.cont.sm.st)
sec_sd<-sd(actint$sec.datpers.sm..mgonl)
mes_sd<-sd(actint$fol.apl.mes.dir)
ach_sd<-sd(actint$pers.ach.in.ult.3.l)

act_sk<-skewness(actint$pers.active.sm)
inf_sk<-skewness(actint$pers.inf.la.b.s)
vand_sk<-skewness(actint$pers.vand.lucr.serv)
cont_sk<-skewness(actint$pers.inf.la.cont.sm.st)
sec_sk<-skewness(actint$sec.datpers.sm..mgonl)
mes_sk<-skewness(actint$fol.apl.mes.dir)
ach_sk<-skewness(actint$pers.ach.in.ult.3.l)
#aplatizari negative si pozitive

act_k<-kurtosis(actint$pers.active.sm)
inf_k<-kurtosis(actint$pers.inf.la.b.s)
vand_k<-kurtosis(actint$pers.vand.lucr.serv)
cont_k<-kurtosis(actint$pers.inf.la.cont.sm.st)
sec_k<-kurtosis(actint$sec.datpers.sm..mgonl)
mes_k<-kurtosis(actint$fol.apl.mes.dir)
ach_k<-kurtosis(actint$pers.ach.in.ult.3.l)
#serii platicurtice si leptocurtice (k>3)

act_cv<-sd(actint$pers.active.sm)/mean(actint$pers.active.sm)
inf_cv<-sd(actint$pers.inf.la.b.s)/mean(actint$pers.inf.la.b.s)
vand_cv<-sd(actint$pers.vand.lucr.serv)/mean(actint$pers.vand.lucr.serv)
cont_cv<-sd(actint$pers.inf.la.cont.sm.st)/mean(actint$pers.inf.la.cont.sm.st)
sec_cv<-sd(actint$sec.datpers.sm..mgonl)/mean(actint$sec.datpers.sm..mgonl)
mes_cv<-sd(actint$fol.apl.mes.dir)/mean(actint$fol.apl.mes.dir)
ach_cv<-sd(actint$pers.ach.in.ult.3.l)/mean(actint$pers.ach.in.ult.3.l)

matrice1<-matrix(nrow = 7 , ncol=4 , dim=list(c("pers.active.sm","pers.inf.b.s","vand.lucr.serv","pers.inf.la.cont.sm.st","sec.datpers.sm..mgonl","fol.apl.mes.dir","pers.ach.in.ult.3.l"),c("Deviatia standard","Skewness","Kurtosis","Coeficientul de variatie")))
matrice1[1,]<-round(c(act_sd , act_sk ,act_k ,act_k),3)
matrice1[2,]<-round(c(inf_sd , inf_sk ,inf_k ,inf_k),3)
matrice1[3,]<-round(c(vand_sd , vand_sk ,vand_k ,vand_k),3)
matrice1[4,]<-round(c(cont_sd , cont_sk ,cont_k ,cont_k),3)
matrice1[5,]<-round(c(sec_sd , sec_sk ,sec_k ,sec_k),3)
matrice1[6,]<-round(c(mes_sd , mes_sk ,mes_k ,mes_k),3)
matrice1[7,]<-round(c(ach_sd , ach_sk ,ach_k ,ach_k),3)


summary(intr)

date_sd<-sd(intr$intr.anal.date.sm)
vanz_sd<-sd(intr$intr.vanz.st.ap.e.com.dif)
prom_sd<-sd(intr$prom.prin.orice.sm)
nrintr_sd<-sd(intr$nr.intr.publ.caut.inf)
CF_sd<-sd(intr$CF.intr.publ.caut.inf)

date_sk<-skewness(intr$intr.anal.date.sm)
vanz_sk<-skewness(intr$intr.vanz.st.ap.e.com.dif)
prom_sk<-skewness(intr$prom.prin.orice.sm)
nrintr_sk<-skewness(intr$nr.intr.publ.caut.inf)
CF_sk<-skewness(intr$CF.intr.publ.caut.inf)

date_k<-kurtosis(intr$intr.anal.date.sm)
vanz_k<-kurtosis(intr$intr.vanz.st.ap.e.com.dif)
prom_k<-kurtosis(intr$prom.prin.orice.sm)
nrintr_k<-kurtosis(intr$nr.intr.publ.caut.inf)
CF_k<-kurtosis(intr$CF.intr.publ.caut.inf)

date_cv<-sd(intr$intr.anal.date.sm)/mean(intr$intr.anal.date.sm)
vanz_cv<-sd(intr$intr.vanz.st.ap.e.com.dif)/mean(intr$intr.vanz.st.ap.e.com.dif)
prom_cv<-sd(intr$prom.prin.orice.sm)/mean(intr$prom.prin.orice.sm)
nrintr_cv<-sd(intr$nr.intr.publ.caut.inf)/mean(intr$nr.intr.publ.caut.inf)
CF_cv<-sd(intr$CF.intr.publ.caut.inf)/mean(intr$CF.intr.publ.caut.inf)

matrice2<-matrix(nrow = 5 , ncol=4 , dim=list(c("intr.anal.date.sm","intr.vanz.st.ap.e.com.dif","prom.prin.orice.sm","nr.intr.publ.caut.inf","CF.intr.publ.caut.inf"),c("Deviatia standard","Skewness","Kurtosis","Coeficientul de variatie")))
matrice2[1,]<-c(date_sd , date_sk ,date_k ,act_k)
matrice2[2,]<-c(vanz_sd , vanz_sk ,vanz_k ,inf_k)
matrice2[3,]<-c(prom_sd , prom_sk ,prom_k ,prom_k)
matrice2[4,]<-c(nrintr_sd , nrintr_sk ,nrintr_k ,nrintr_k)
matrice2[5,]<-c(CF_sd , CF_sk ,CF_k ,CF_k)

date_finale<-date_finale[-21,]
is_outlier <- function(date_finale) {
  Q1 <- quantile(date_finale, 0.25)
  Q3 <- quantile(date_finale, 0.75)
  IQR_value <- IQR(date_finale)
  (date_finale< (Q1 - 1.5 * IQR_value)) | (date_finale> (Q3 + 1.5 * IQR_value))
}
outliers <- date_finale[,-1] %>% mutate(across(everything(), is_outlier))
rows_with_outliers <- rowSums(outliers) > 0
date_finale_no_outliers <- date_finale[!rows_with_outliers, ]
date_finale_no_outliers
date_finale<-date_finale_no_outliers
date_finale
View(date_finale)
colSums(outliers)
str(date_finale)


X<-date_finale[,c(1:8)]
rownames(X) <- X[,1]
X_std = scale(X[,-1], center = T, scale = T)
acp <- princomp(X_std,cor=T,scores=T)
summary(acp)
scoruri <- acp$scores
round(cor(scoruri),3)

as <- acp$sdev
var <- as*as
var
sum(var)
coef <- acp$loadings 
scoruri <- acp$scores
round(cor(scoruri),3)
summary(acp)
plot(acp,type="l")
print(acp)


R<- cor(X_std) 
corrplot(R, method = "square", type = "lower")

E <- eigen(R) 
V <- E$vectors 
lambda<- E$values 
sqrt(lambda) # abaterile standard ale componentelor principale 
comblin <- scale(X_std,center=T,scale=T)%*%coef # sunt scorurile
MF=cor(X_std,scoruri)
MF
corrplot(MF, method = "number")

#Z1->informarea si activitatea economica  a tarilor in mediul online
#Z2<-Activitatea persoanelor in social media

coef
round(V,3)
biplot(acp,cex = 0.6)
X_std[1:2,]
scoruri[1:2,]
acp

scoruri <- scoruri[,1:2] 
colnames(scoruri) <- c("informarea si activitatea comerciala a tarilor in mediul online", "Activitatea persoanelor in social media")
scoruri


X1<-date_finale[,c(1,9:13)]
rownames(X1) <- X1[,1] 
X1_std = scale(X1[,-1], center = T, scale = T)
acp1 <- princomp(X1_std,cor=F,scores=T)
summary(acp1)
#avem 2 comp principale

coef1<- acp1$loadings 
coef1
scoruri1 <- acp1$scores
round(cor(scoruri1),3)


R1<- cor(X1_std) 
corrplot(R1, method = "square", type = "lower")

E1 <- eigen(R1) 
V1 <- E1$vectors 
lambda1<- E1$values 
sqrt(lambda1) 
comblin1 <- scale(X1_std,center=F,scale=T)%*%coef1 
MF1=cor(X1_std,scoruri1)
MF1
corrplot(MF1, method = "number")

#Z1<-Adopția digitală a companiilor în promovare și comerț
#Z2->Promovari si campanii de marketing ale intreppinderilor

biplot(acp1, cex = 0.6)
X1_std[1:2,]
scoruri1[1:2,]
acp1

scoruri1 <- scoruri1[,1:2] 
colnames(scoruri1) <- c("Adoptia digitală a companiilor în promovare si comert", "Promovari si campanii de marketing ale intreppinderilor")
scoruri1



Scoruri_finale<-cbind(scoruri,scoruri1)
Scoruri_finale


# Ward.D2 -----------------------------------------------------------------


d <- dist(Scoruri_finale)
ierarhie <- hclust(d,method="ward.D2")
plot(ierarhie, main = "Dendrograma clusterelor - Metoda Ward.D2", xlab="Țările" , ylab="Înalțimea", cex = 0.8)
ierarhie$height

nr<- NbClust(Scoruri_finale, distance="euclidean", min.nc=2, max.nc=7, method="ward.D2", index="all")
nr$All.index
nr$Best.nc
#Nr optim de clustere este de 5

solutie <- cutree(ierarhie,k=5)
table(solutie)
#Nr optim de clustere este de 2
#Cl1-12 ,Cl2-13 
Scoruri_finale <- as.data.frame(Scoruri_finale)
Scoruri_finale$Cluster <- as.factor(solutie)

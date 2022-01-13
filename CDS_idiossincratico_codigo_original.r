getwd()
#setwd("C:/Users/marce/OneDrive/Área de Trabalho/Ipea/CDS")

rm(list=ls())

library(xts)
library(dynlm)
library(forecast)
library(glmnet)
#library(HDeconometrics)

#############
# DADOS CDS
#############

#dados<-read.csv('CDS_GP_21_09_2018_tratado.csv',sep=";",dec=",",header=T)
#dados<-read.csv('CDS_GP_14_12_2018_tratado.csv',sep=";",dec=",",header=T)
dados<-read.csv('CDS_tratado.csv',sep=";",dec=",",header=T)
#dados<-read.csv('CDS_05-11-19.csv',sep=";",dec=",",header=T)
# inicio = 06-10-2000
dados=xts(dados[,-1],as.Date(dados[,1],format="%m/%d/20%y"))
dados=na.approx(dados)
head(dados)
dados[,1]


# define subamostras
# dados_1: 2005-18
# dados_2: 2008-18
# dados_3: 2009-18
# dados_4: 2013-18
# Argentina e Grecia retiradas de todas as subamostras
dados_1=dados[,-c(10,13,14,16,17,20,24,26)]
dados_2=dados[,-c(10,13,14,16)]
dados_3=dados[,-c(10,14,16)]
dados_4=dados[,-c(10,14)]
head(dados_2)

n1=ncol(dados_1)
n2=ncol(dados_2)
n3=ncol(dados_3)
n4=ncol(dados_4)


#############
# JUNTA DADOS
#############

#usar este comando para amostra restrita pelos CDS
dados1=na.omit(dados_1)
dados2=na.omit(dados_2)
dados3=na.omit(dados_3)
dados4=na.omit(dados_4)

head(dados2)
cor(dados2)

CDSBRA1=dados1[,1]
CDSBRA1p=CDSBRA1/sd(CDSBRA1)
CDSBRA2=dados2[,1]
CDSBRA2p=CDSBRA2/sd(CDSBRA2)
CDSBRA3=dados3[,1]
CDSBRA3p=CDSBRA3/sd(CDSBRA3)
CDSBRA4=dados4[,1]
CDSBRA4p=CDSBRA4/sd(CDSBRA4)

CDSX1=dados1[,2:n1]
CDSX2=dados2[,2:n2]
CDSX3=dados3[,2:n3]
CDSX4=dados4[,2:n4]
head(CDSX2)

ts.plot(CDSX1,col=1:(n1-1))
ts.plot(cbind(CDSX1,CDSBRA1),col=c(rep(3,(n1-1)),1))
ts.plot(cbind(CDSX2,CDSBRA2),col=c(rep(3,(n2-1)),1))
ts.plot(cbind(CDSX3,CDSBRA3),col=c(rep(3,(n3-1)),1))
ts.plot(cbind(CDSX4,CDSBRA4),col=c(rep(3,(n4-1)),1))


# seleciona amostra
CDSBRA=CDSBRA2
CDSBRAp=CDSBRA2p
CDSX=CDSX2
dadosx=dados2
nx=n2

#define intervalo
per=start(dadosx)
miny=min(window(CDSX,start=per),window(CDSBRA,start=per))
maxy=max(window(CDSX,start=per),window(CDSBRA,start=per))

##############
#   PCA - CDS - dados2
##############

CDSX=CDSX2
dadosx=dados2

# dados CDSX padronizados
CDSXp=matrix(NA,nrow=nrow(CDSX),ncol=ncol(CDSX))
for (i in 1:ncol(CDSXp)) {
  CDSXp[,i]=CDSX[,i]/sd(CDSX[,i])
}
CDSXp=xts(CDSXp,as.Date(index(dadosx),format="%d/%m/20%y"))

# componentes principais com dados previamente padronizados
a=prcomp(CDSXp,scale. = T)
summary(a)
pc21=xts(a$x[,1],as.Date(index(dadosx),format="%d/%m/20%y"))
pc22=xts(a$x[,2],as.Date(index(dadosx),format="%d/%m/20%y"))
pc23=xts(a$x[,3],as.Date(index(dadosx),format="%d/%m/20%y"))

#####################
# CP CDSX x pc (*******)
#####################
per='2017-01-01'
miny=min(window(CDSX2,start=per))
maxy=max(window(CDSX2,start=per))
maxy=280
plot(window(CDSX2,start=per),type='n',ylim=c(miny,maxy),main="")
for(i in 1:(n2-1) ){
  lines(window(CDSX2[,i],start=per),col=3,lwd=2)
}
par(new=T)
plot(window(-pc21,start=per),lwd=3,ylim=c(-6,0),axes=F,main="")
title(main="CDS-Emergentes: 22 paises x 1.componente principal ")
legend("topright",c("1.CP","CDS"),col=c(1,3),lwd=2,bty="n")

# exporta dados grafico
dadosexp=cbind(CDSX2,-pc21)
write.csv2(dadosexp, file = "dados_CDSX_25-11-19_jpc.csv")
dadosexp=as.Date(index(pc21))
write.csv2(dadosexp, file = "dados_CDSX_data_25-11-19_jpc.csv")

##############
#  REGRESSAO
##############
reg2=lm(CDSBRA2~pc21+pc22+pc23)
fit2=fitted(reg2)
resid2=resid(reg2)

# exporta dados grafico
dadosexp=cbind(CDSBRA2,fit2,resid2)
write.csv(dadosexp, file = "dados_risco_bra_25-11-19_jpc.csv")
dadosexp=as.Date(index(resid2))
write.csv(dadosexp, file = "dados_risco_bra_data_25-11-19_jpc.csv")
colnames(CDSX2)




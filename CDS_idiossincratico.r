#Rotina para calcular o CDS idiossincrático para o Brasil
#Adaptado por: Marcelo Vilas Boas de Castro
#última atualização: 10/11/2020

#Definindo diretórios a serem utilizados
getwd()
setwd("C:/Users/User/Documents")

#Carregando pacotes que serão utilizados
library(xts)
library(dynlm)
library(timeDate)
library(rio)

#Importando dados CDS
dados = read.csv('CDS_tratado.csv',sep=";",dec=",",header=T)
dados[,1] = as.Date(dados[,1],format="%d-%m-%y") #Formatando coluna de datas
dados[,-1] = na.approx(dados[,-1]) #Faz uma interpolação para tirar NA's dos dados

#Definindo subamostra
dados_s = dados[,-c(11,15)] #Argentina e Grécia retiradas da subamostra
dados_s = na.omit(dados_s) #Tirando NA's que sobraram na subamostra
n_col_dados = ncol(dados_s)
CDSBRA_semdata = dados_s[,2]
CDS_semdata = dados_s[,2:n_col_dados]

#Padronizando CDS
CDS_semdatap=matrix(NA,nrow=nrow(CDS_semdata),ncol=ncol(CDS_semdata)) #Cria dataframe vazio do tamanho do CDS_semdata
for (i in 1:ncol(CDS_semdatap)) {
  CDS_semdatap[,i]=CDS_semdata[,i]/sd(CDS_semdata[,i])
}
CDS_semdatap = as.data.frame(CDS_semdatap)

#Calculando componentes principais com dados previamente padronizados
pc=prcomp(CDS_semdatap,scale. = T)
summary(pc)

#Escolheu-se usar 3 componentes, por explicarem quase 90% da amostra
for (i in 1:3){
  pc_for=xts(pc$x[,i],as.Date(dados_s[,1],format="%d/%m/%Y"))
  nome_arquivo = paste("pc", i, sep = "")
  assign(nome_arquivo, pc_for)
}

#Regressão
reg=lm(CDSBRA_semdata~pc1+pc2+pc3)
fit=fitted(reg)
resid=resid(reg)

#Exportação de dados
data_dados=as.Date(index(pc1))
dados_exp=cbind(data_dados, CDS_semdata,-pc1)
colnames(dados_exp) = c("Data", "Brasil", "Indonésia", "Turquia", "Colômbia", "Peru", "Hungria",	"Coreia do Sul",
                         "Malásia",	"Filipinas", "Bulgaria",	"Croacia", "Islândia",	"Irlanda",	"México",	"Chile",
                         "Marrocos",	"Polônia",	"Rússia",	"África do Sul",	"Eslovênia",	"Tailândia",	"Butão",
                         "Vietnam", "Primeiro componente principal *(-1)")
write.csv2(dados_exp, file = "dados_CDS.csv", row.names = F)
export(dados_exp, "CDS.xlsx", sheetName = "dados_CDS")

dados_exp2=as.data.frame(cbind(CDSBRA_semdata,fit,resid))
dados_exp2=cbind(data_dados, dados_exp2)
colnames(dados_exp2) = c("Data", "CDS Brasil", "Ajustado", "Resíduo")
write.csv2(dados_exp2, file = "dados_risco_bra.csv", row.names = F)
export(dados_exp2, "CDS.xlsx", which = "dados_risco_bra")
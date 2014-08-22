#' 1 #### dados da Ilana
library("foreign")
myData <- read.spss("~/Dropbox/POF Ilana/alimentogrnovo.sav")
summary(myData)
summary(myData$LOCAL)
summary(myData$QTD_IMPUT)
summary(myData$uf)
summary(myData$idadeano)

#' #### 1. carregar pacote com dicionários
require("dicionariosIBGE")
data(package = "dicionariosIBGE")
data(dicPOF2008.2009)

#' #### 2. manipular dados

# cria lista com rotulos
rotulos <- split(x = rot16pof2008.2009,f = rot16pof2008.2009$cod)
#ler microdados
consumo <- read.fwf("~/Google Drive/pof - aalane/Dados/T_CONSUMO_S.txt", widths = dic16pof2008.2009$tamanho)
# renomar colunas
colnames (consumo)<-dic16pof2008.2009$cod
# ver microdados com nomes de colunas
View (consumo[1:10,])

#Vtest <-merge(x = consumo, y = rotulos$COD_UF, by.x = "COD_UF" ,by.y = "valor")

consumo$COD_UF<-apply(consumo, 1 , function(aa) rotulos$COD_UF$rotulo[match (aa["COD_UF"], rotulos$COD_UF$valor)])
consumo$COD_UF<-as.factor(consumo$COD_UF)

consumo$COD_ITEM<-apply(consumo, 1 , function(aa) rotulos$COD_ITEM$rotulo[match (aa["COD_ITEM"], rotulos$COD_ITEM$valor)])
consumo$COD_ITEM<-as.factor(consumo$COD_ITEM)
#' save first 1000 lines
write.csv(consumo[1:1000,], "~/Google Drive/pof - aalane/POF-R/consumo-1000linhas.csv")

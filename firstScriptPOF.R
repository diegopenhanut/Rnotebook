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

#consumo$COD_ITEM<-apply(consumo, 1 , function(aa) rotulos$COD_ITEM$rotulo[match (aa["COD_ITEM"], rotulos$COD_ITEM$valor)])
#consumo$COD_ITEM<-as.factor(consumo$COD_ITEM)

#' save first 1000 lines
write.csv(consumo[1:1000,], "~/Google Drive/pof - aalane/POF-R/consumo-1000linhas.csv")

#' tabela completa URL
#' ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2008_2009/Tabelas_de_Composicao_Nutricional_dos_Alimentos_Consumidos_no_Brasil/tabelacompleta.zip
compCentesimal<-read.table(file = "~/Google Drive/pof - aalane/tabelacompleta.txt",
  skip = 3, sep = "\t", header = TRUE, fileEncoding =  "latin1", na.strings="-")
#' drop blank fields
compCentesimal<-compCentesimal[,1:43]
#' merge
compInd <- merge(x = consumo, y = compCentesimal, by.x = c("COD_ITEM", "COD_PREPARACAO"),
  by.y = c("CîDIGO.DO.ALIMENTO", "CîDIGO............DA.PREPARA.ÌO"), all.x = TRUE, all.y = FALSE)

View(compInd[1:10,])

df <- data.frame(data.frame(unlist(compInd[1:10,]), nrow=67, byrow=T))

#require("plyr")


compInd<-as.data.frame(compInd)

#' basic math

as.numeric(compInd$QTD_FINAL)/100*as.numeric(compInd$ENERGIA..kcal.)
lala<-as.vector(compInd[1:10,31:length(compInd)])*as.numeric(compInd$QTD_FINAL)

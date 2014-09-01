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

#df <- data.frame(data.frame(unlist(compInd[1:10,]), nrow=67, byrow=T))

#require("plyr")


compInd<-as.data.frame(compInd)

#' basic math

compInd$ENERGIA..kcal.<-as.numeric(compInd$QTD_FINAL)/100*as.numeric(compInd$ENERGIA..kcal.)
compInd$PROTEêNA..g.<-as.numeric(compInd$QTD_FINAL)/100*as.numeric(compInd$PROTEêNA..g.)
compInd$LIPêDEOS.TOTAIS..g.<-as.numeric(compInd$QTD_FINAL)/100*as.numeric(compInd$LIPêDEOS.TOTAIS..g.)
compInd$CARBOIDRATO..g.<-as.numeric(compInd$QTD_FINAL)/100*as.numeric(compInd$CARBOIDRATO..g.)

#' check the number of persons
length(levels(as.factor(paste(compInd$COD_UF, compInd$NUM_SEQ, compInd$NUM_DV, compInd$COD_DOMC,
  compInd$NUM_UC, compInd$NUM_INFORMANTE, sep = " - "))))

#' add a ID field
compInd$ID <- as.factor(paste(compInd$COD_UF, compInd$NUM_SEQ, compInd$NUM_DV, compInd$COD_DOMC,
  compInd$NUM_UC, compInd$NUM_INFORMANTE, sep = " - "))

compInd$ID <- as.numeric(compInd$ID)
#max(compInd$ID)
#summary(compInd$ID)

firstMacro<-compInd[,c(1:34, length(compInd) )]
View(firstMacro[1:10,])

#' num do quadro = 71 primeiro dia, 72 seagundo dia.
myData<-aggregate(x = firstMacro[,31:35] , by=list( firstMacro$COD_UF, firstMacro$ID, firstMacro$NUM_QUADRO), FUN="sum",  na.rm=TRUE)
View(myData)
require("ggplot2")

qplot( y = myData$ENERGIA..kcal., x=paste(myData$Group.1,myData$Group.3, sep = ", dia "),
 geom = "boxplot", main="Energia", ylab="Kcal")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

qplot( y = myData$PROTEêNA..g., x=paste(myData$Group.1,myData$Group.3, sep = ", dia "),
       geom = "boxplot", main="Proteína", ylab="g")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

qplot( y = myData$LIPêDEOS.TOTAIS..g., x=paste(myData$Group.1,myData$Group.3, sep = ", dia "),
       geom = "boxplot", main="Lipídeos", ylab="g")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

qplot( y = myData$CARBOIDRATO..g., x=paste(myData$Group.1,myData$Group.3, sep = ", dia "),
       geom = "boxplot", main = "Carboidratos", ylab="g")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

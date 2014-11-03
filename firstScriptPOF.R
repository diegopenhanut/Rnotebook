#' ---
#' title: "Consumo Alimentar e dataSus"
#' author: Emanuel Diego S Penha
#' date: Oct 19, 2014
#' output:
#' html_document:
#' toc: true
#' pdf_document:
#'   toc: true
#' ---

#' turn on cache
knitr::opts_chunk$set(cache=TRUE)
#' 

#' #### 1. ler tabelas do datasus
datasusEstFem<-read.table(file = "data/datasusEstFem.txt",
  sep = "\t", header = TRUE, fileEncoding =  "mac", na.strings="-")
datasusEstMas<-read.table(file = "data/datasusEstMas.txt",
  sep = "\t", header = TRUE, fileEncoding =  "mac", na.strings="-")
datasusRegFem<-read.table(file = "data/datasusRegFem.txt",
  sep = "\t", header = TRUE, fileEncoding =  "mac", na.strings="-")
datasusRegMas<-read.table(file = "data/datasusRegMas.txt",
  sep = "\t", header = TRUE, fileEncoding =  "mac", na.strings="-")

#' #### 2. carregar pacote com dicionários
require("dicionariosIBGE")
data(package = "dicionariosIBGE")
data(dicPOF2008.2009)

#' #### 3. carregar dados de morador
morador <- read.fwf("~/Google Drive/pof - aalane/Dados/T_MORADOR_S.txt", widths = dic2pof2008.2009$tamanho)
colnames(morador)<-dic2pof2008.2009$cod
head(morador)

#' #### 4. formatar dados de consumo

#' criar lista com rotulos
rotulos <- split(x = rot16pof2008.2009,f = rot16pof2008.2009$cod)
#' ler microdados
consumo <- read.fwf("~/Google Drive/pof - aalane/Dados/T_CONSUMO_S.txt", widths = dic16pof2008.2009$tamanho)
#' renomar colunas
colnames (consumo)<-dic16pof2008.2009$cod
#' ver microdados com nomes de colunas
head (consumo)

#' substituir COD_UF pelo rótulo
#consumo$COD_UF<-apply(consumo, 1 , function(aa) rotulos$COD_UF$rotulo[match (aa["COD_UF"], rotulos$COD_UF$valor)])
#consumo$COD_UF<-as.factor(consumo$COD_UF)

#' #### 5. carregar tabela de composição usada na POF
#' tabela completa URL
#' ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2008_2009/Tabelas_de_Composicao_Nutricional_dos_Alimentos_Consumidos_no_Brasil/tabelacompleta.zip
compCentesimal<-read.table(file = "~/Google Drive/pof - aalane/tabelacompleta.txt",
  skip = 3, sep = "\t", header = TRUE, fileEncoding =  "mac", na.strings="-")
#' drop blank fields
summary(compCentesimal)
compCentesimal<-compCentesimal[,1:43]

#' #### 6. Merge composição e consumo para obter insgestão de nutrientes.
compInd <- merge(x = consumo, y = compCentesimal, by.x = c("COD_ITEM", "COD_PREPARACAO"),
  by.y = c("CÓDIGO.DO.ALIMENTO", "CÓDIGO............DA.PREPARAÇÃO"), all.x = TRUE, all.y = FALSE)

head (compInd)
compInd<-as.data.frame(compInd)

#' regra de 3 para obter ingestão por indivíduo/dia para macronutrientes. 
regraDeTres<-function(numeratorList, denominator ){
  lapply(numeratorList, function(a){
    as.numeric(denominator)/100*as.numeric(a)
  })
}
#' ver nomes dos itens na lista
names(compInd)
#' usar somente as variaveis alimentares
compInd[,31:67]<-regraDeTres(compInd[,31:67], compInd$QTD_FINAL)

#' verificar o número de pessoas
length(levels(as.factor(paste(compInd$COD_UF, compInd$NUM_SEQ, compInd$NUM_DV, 
  compInd$COD_DOMC, compInd$NUM_UC, compInd$NUM_INFORMANTE, sep = " - "))))

#' add a ID field
compInd$ID <- as.factor(paste(compInd$COD_UF, compInd$NUM_SEQ, compInd$NUM_DV,
  compInd$COD_DOMC, compInd$NUM_UC, compInd$NUM_INFORMANTE, sep = " - "))

head(compInd$ID)
#' #### 7 merge com os dados de moradores
morador$ID <- as.factor(paste(morador$COD_UF, morador$NUM_SEQ, morador$NUM_DV,
  morador$COD_DOMC, morador$NUM_UC, morador$NUM_INFORMANTE, sep = " - "))

head(morador$ID)

#' merge somente idade sexo e ID
moradorConsumo <- merge(x = compInd, y = morador[c("ID", "COD_SEXO", "IDADE_ANOS")], all.x = TRUE)

names(moradorConsumo)
head(moradorConsumo)

#' num do quadro = 71 para primeiro dia, 72 para seagundo dia.
moradorConsumoPorDia<-aggregate(x = moradorConsumo[,32:68] , by=list( moradorConsumo$COD_UF, 
  moradorConsumo$ID, moradorConsumo$NUM_QUADRO), FUN="sum",  na.rm=TRUE)

#' merge novamente somente idade sexo e ID (tenho que concertar isso mais tarde)
moradorConsumoPorDia <- merge(x = moradorConsumoPorDia, y = morador[c("ID", "COD_SEXO", "IDADE_ANOS")],
  by.x = "Group.2", by.y = "ID", all.x = TRUE)

#' substituir COD_UF pelo rótulo
moradorConsumoPorDia$Group.1<-apply(moradorConsumoPorDia, 1 , function(aa) {
  rotulos$COD_UF$rotulo[match (aa["Group.1"], rotulos$COD_UF$valor)]
})
moradorConsumoPorDia$Group.1<-as.factor(moradorConsumoPorDia$Group.1)
moradorConsumoPorDia$classIDADE_ANOS<-cut(x = moradorConsumoPorDia$IDADE_ANOS, breaks = c(18, 30, 50, 70, Inf))

#' consumo por estado
moradorConsumoPorDiaAgregado<-apply(moradorConsumoPorDia[4:(length(moradorConsumoPorDia)-3)], MARGIN = (2),
  FUN = function(a) {
    aggregate(a ~ COD_SEXO + Group.1 + classIDADE_ANOS, data = moradorConsumoPorDia, FUN= "mean" )
  })


#' change labels to match intake data
levels(datasusEstFem$Doenças..Faixas.Etárias.DRI)
levels(moradorConsumoPorDiaAgregado$ENERGIA..kcal.$classIDADE_ANOS)
levels(datasusEstFem$Doenças..Faixas.Etárias.DRI)<-c ("(70,Inf]" , "(18,30]" , "(30,50]" , "(50,70]" , NA)
levels(datasusEstMas$Doenças..Faixas.Etárias.DRI)<-c ("(70,Inf]" , "(18,30]" , "(30,50]" , "(50,70]" , NA)
levels(datasusEstFem$estado)<-levels(moradorConsumoPorDia$Group.1)
levels(datasusEstMas$estado)<-levels(moradorConsumoPorDia$Group.1)

## ---- last piece of code. Heavy one.
#' #### 8 Média de consumo por estado, sexo, faixa etária
#moradorConsumoPorDiaAgregado
moradorConsumoPorDiaAgregado<-lapply(moradorConsumoPorDiaAgregado, function(a){
  split(a,a$COD_SEXO)
})
#View(moradorConsumoPorDiaAgregado)
# COD_SEX = 1 para homens e 2 para mulheres
moradorConsumoPorDiaAgregadoFem<-lapply(moradorConsumoPorDiaAgregado , function(a){
  merge(x=a$`2`, y=datasusEstFem, by.x = c("Group.1", "classIDADE_ANOS"),
    by.y=c("estado", "Doenças..Faixas.Etárias.DRI"))
    })
moradorConsumoPorDiaAgregadoMas<-lapply(moradorConsumoPorDiaAgregado , function(a){
  merge(x=a$`1`, y=datasusEstMas, by.x = c("Group.1", "classIDADE_ANOS"),
        by.y=c("estado", "Doenças..Faixas.Etárias.DRI"))
})

moradorConsumoPorDiaAgregadoFem<- lapply(moradorConsumoPorDiaAgregadoFem, function (a){
  split(a, a$classIDADE_ANOS)
})

moradorConsumoPorDiaAgregadoMas<- lapply(moradorConsumoPorDiaAgregadoMas, function (a){
  split(a, a$classIDADE_ANOS)
})


library("Hmisc")
correlacoesFem <- lapply(moradorConsumoPorDiaAgregadoFem, function(g) {
  lapply(g, function(a) {
    rcorr(as.matrix(a[4:length(a)]))
  })
})
correlacoesMas <- lapply(moradorConsumoPorDiaAgregadoMas, function(g) {
  lapply(g, function(a) {
    rcorr(as.matrix(a[4:length(a)]))
  })
})

correlacoesFem
correlacoesMas

#View(correlacoesFem$ENERGIA..kcal.$`(18,30]`$P[1,])

correlacoesFem<-lapply(correlacoesFem, function(g){
  lapply(g, function(a){
    lapply(a, function(i){
      i[1,]
    })
  })
})

correlacoesMas<-lapply(correlacoesMas, function(g){
  lapply(g, function(a){
    lapply(a, function(i){
      i[1,]
    })
  })
})


correlacoesFem
correlacoesMas

write.csv(x = as.data.frame(correlacoesFem), file = "correlacoesFem.csv")
write.csv(x = as.data.frame(correlacoesMas), file = "correlacoesMas.csv")

library("VIM")
matrixplot(correlacoesFem)
lala<-na.omit(as.data.frame(correlacoesFem))
lelep<-seq(from = 3, by = 3, to = length(lala))
leler<-seq(from = 1, by = 3, to = length(lala))
View(as.matrix(lala[,lele])) 
matrixplot(as.matrix(lala[,lele]), )
heatmap(as.matrix(lala[,lelep]), na.rm = FALSE,margins = c(15,15))
heatmap(as.matrix(lala[,leler]), na.rm = FALSE,margins = c(15,15))
matrixplot(as.matrix(lala[,leler]), na.rm = FALSE,margins = c(15,15))
matrix

####################################################################################################
###                                   |||Dependency Hunter|||                                    ###
###    Criado com o intuito de extrair as dependecias entre os pacotes em aplicacoes HEMPS.      ###
###                                                                                              ###
### Desenvolvido por Raphael Brasil e Weslley Nojosa                                             ###
### IFCE - Campus Marancanaú, 2017                                                               ###
####################################################################################################



library(readxl)
packet <- read_excel("/home/weslley/Downloads/hempsDTW.xlsx") ### Local onde fica o arquivo com os dados da simulacao.
nodeTarget  <- 2	### Nó alvo da extração das dependências.(Devemos depois criar um laco onde passe por todos os nos da rede)
targetNode <- packet[packet$Target == nodeTarget, ]  ### Todos as linhas com Target == nodeTarget  
sourceNode <- packet[packet$Source == nodeTarget, ]  ### Todos as linhas com Source == nodeTarget
mergedTable <- merge(x = targetNode, y = sourceNode, ### Uniao das duas tabelas aneriores.
                     by = c("Timestamp","Source","CurrentNode","Service","Payload","Target"), all = TRUE)

refinedData <- subset(mergedTable, !(Source != CurrentNode & CurrentNode != Target)) ### Mostra somente as linhas com o pacote a ser enviado, ou pacotes recebidos.
refinedData <- subset(refinedData, (CurrentNode != Target))
#UniqueValues <- unique(refinedData[,-1])	 ### Data Frame com as informacoes de cada pacote unico.
#UniqueValues <- refinedData[refinedData$Service==20 | refinedData$Service == 10,] 
UniqueValues <- refinedData[refinedData$Service==20, ] 	 ### Utilizando todos os nos.
UniqueValues$TAG <- 0																     ### Cria uma nova coluna, onde colocaremos os valores da TAG.
numRow<- NROW(UniqueValues)															 ### Numero de linhas que o DF contem.
varTAG <- 1																		        	 ### Variavel auxiliar para a funcao de TAG

####################################################################################################
################################## FUNCAO QUE INSERE A TAG #########################################
for(i in 1:(numRow)){	
  if(UniqueValues$TAG[i] == 0){
    if(UniqueValues$Source[i] != nodeTarget){
      UniqueValues$TAG[i] <- paste0("I",varTAG)
    }
    
    for(y in (i+1):(numRow)){
      if((UniqueValues$Source[y]  == UniqueValues$Source[i])  & 
         (UniqueValues$Service[y] == UniqueValues$Service[i]) & 
         (UniqueValues$Payload[y] == UniqueValues$Payload[i]) & 
         (UniqueValues$Target[y]  == UniqueValues$Target[i])){
        UniqueValues$TAG[y] <- paste0("I",varTAG)
      }
    } 
  }
  varTAG <- varTAG + 1 
}
varTAG <- 1
for(i in 1:(numRow)){
  if(UniqueValues$TAG[i] == 0){
    UniqueValues$TAG[i] <- paste0("O",varTAG)
    for(y in (i+1):(numRow)){
      if((UniqueValues$Source[y]  == UniqueValues$Source[i])  & 
         (UniqueValues$Service[y] == UniqueValues$Service[i]) & 
         (UniqueValues$Payload[y] == UniqueValues$Payload[i]) & 
         (UniqueValues$Target[y]  == UniqueValues$Target[i])){
        UniqueValues$TAG[y] <- paste0("O",varTAG)
      }
    }
    varTAG <- varTAG + 1
  }
}		

R <-  unlist(UniqueValues$TAG)
R <- gsub("[O][1]","", R)
R1 <- R[R!=""]
temp <- table(as.array(R1))
temp
names(temp)[temp == max(temp)]

####################################################################################################
################################## FUNCAO QUE INSERE A TAG #########################################


####################################################################################################
############################## FUNCAO QUE IDENTIFICA AS DEP ########################################




###Como fazer um vetor de dataframes###
FOO <- list()
days <- 3
for (i in 1:days)
{
  FOO[[i]] <- data.frame(x=c(i, i+1, i+2), y=c(i, i*i, i*i*i))
}

























Teste <- kmeans(UniqueValues[1:6], 2)
Teste
Teste$size 

plot(UniqueValues[2:5], col = UniqueValues$Service, pch= 19)
plot(packet[1:6], col = Teste$cluster, pch= 19)



###################################### FUNCAO QUE CALCULA A QUANTIDADE DE DEPENDECIAS ###############
a <- as.vector(t(refinedData$Timestamp))                ### TranspÃµe os valores da coluna Timestamp no nÃ³ 1
b <- as.vector(t(refinedData$Timestamp))                ### TranspÃµe os valores da coluna Timestamp no nÃ³ 2
###nodeTarget <- packet[packet$Target == 0, ]    ### Linhas com target igual ao segundo nÃ³
MACROA <- length(a)
MACROB <- length(b)
### Valor MACROdependecy com tamanho do vetor de dependecy, deve ter tamanho igual ao nÃºmero de pacotes enviados pelo segundo nÃ³.
if (MACROB < MACROA){
  MACROdependecy <- MACROB
} else{
  MACROdependecy <- MACROA
}
dependecy <-rep(0,MACROdependecy)     ### Popula o vetor dependecy com zero
### LaÃ§o que captura TODAS as possÃ�veis dependencias entre dois nÃ³s, sem filtragem.
for(i in 1:MACROB){                               ### NÃºmeros de pacotes do nÃ³ B
  for(j in 1:MACROA){                             ### NÃºmeros de pacotes do nÃ³ B
    if(a[j] < b[i]){                              ### VÃª todos os pacotes do nÃ³ A que foram enviados antes do nÃ³ B enviar os pacotes
      ### Colocar pacote que originou a dependencia
      dependecy[i] <- dependecy[i] + 1            ### Calcula o nÃºmero de possiveis dependecias entre cada pacote enviado pelo nÃ³ B     
    }
  }
}
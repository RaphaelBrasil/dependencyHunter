####################################################################################################
###                                   |||Dependency Hunter|||                                    ###
###    Criado com o intuito de extrair as dependecias entre os pacotes em aplicacoes HEMPS.      ###
###                                                                                              ###
### Desenvolvido por Raphael Brasil e Weslley Nojosa                                             ###
### IFCE - Campus Marancanaú, 2017                                                               ###
####################################################################################################



library(readxl)
library(stringr)
packet <- read_excel("C:/Users/Weslley/Downloads/hempsDTW.xlsx") ### Local onde fica o arquivo com os dados da simulacao.
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
    UniqueValues$TAG[i] <- varTAG
    for(y in (i+1):(numRow)){
      if((UniqueValues$Source[y]  == UniqueValues$Source[i])  & 
         (UniqueValues$Service[y] == UniqueValues$Service[i]) & 
         (UniqueValues$Payload[y] == UniqueValues$Payload[i]) & 
         (UniqueValues$Target[y]  == UniqueValues$Target[i])){
        UniqueValues$TAG[y] <- varTAG
      }
    }
    varTAG <- varTAG + 1
  }
}		

# R <-  unlist(UniqueValues$TAG)
# R <- gsub("[1]","", R)
# R1 <- R[R!=""]
# temp <- table(as.array(R1))
# temp
# names(temp)[temp == max(temp)]



G <- unlist(UniqueValues$TAG)
g <- length(G)
G[[17]] <- 2
G[[20]]<- 3
G[[6]]<- 4
G
aux <- 1 #head(G[[1]])
matrixG <- matrix(G,nrow = g, ncol = g)
for(i in 1:(g)){
  if(str_detect(G[[i]], "^[0-9]+$")==TRUE){
    for(j in aux:i){
      rowMatrix <- strtoi(G[[i]], base = 0L) #Transforma a string em numeric
      matrixG[rowMatrix,j] <- G[[aux]]
      aux <- aux + 1
    }
    j <- i + 1
  }
}



G[[3]] <- 100
G[[3]]
padrao <- "^[0-9]+$"
#G[3]==grep("^[0-9]+$",padrao)
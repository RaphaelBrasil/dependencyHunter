####################################################################################################
###                                   |||Dependency Hunter|||                                    ###
###    Criado com o intuito de extrair as dependecias entre os pacotes em aplicacoes HEMPS.      ###
###                                                                                              ###
### Desenvolvido por Raphael Brasil e Weslley Nojosa                                             ###
### IFCE - Campus Marancana?, 2017                                                               ###
####################################################################################################



library(readxl)
library(stringr)
library(arules)
library(devtools)
library(arulesViz)
packet <- read_excel("/home/weslley/Documentos/Github/dependencyHunter/APPs/hempsApptest.xlsx") ### Local onde fica o arquivo com os dados da simulacao.
nodeTarget  <- 512	### N? alvo da extra??o das depend?ncias.(Devemos depois criar um laco onde passe por todos os nos da rede)
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
G <- unlist(UniqueValues$TAG)     #Cria uma lista com a coluna das TAGs
g <- length(G)                    #vari?vel para receber o comprimento da lista
aux <- 1 #head(G[[1]])
l <- list()
for(i in 1:(g)){                              #La?o que percorre a lista verificando a quantidade de sa?das       
  if(str_detect(G[[i]], "^[0-9]+$")==TRUE){   #e criando um espa?o para cada sa?da correspondente. ex: out = 1, l[[1]]
    for(j in aux:i){
      rowOut <- strtoi(G[[i]], base = 0L)   #Transforma a string em numeric
      l[[rowOut]] <- G[[j]]
    }
    aux <- i 
  }                     
}                        
aux2 <- 1
for(i in 1:(g)){           #La?o auxiliar que cria listas na lista original, inserindo cada entrada na sua chave/sa?da 
  if(str_detect(G[[i]], "^[0-9]+$")==TRUE){
    rowOut <- strtoi(G[[i]], base = 0L)
    while(aux2!=i+1){
      if(is.null(l[[rowOut]])==FALSE){
        l[[rowOut]] <-c(l[[rowOut]],G[[aux2]])
      }
      aux2 <- aux2 +1
    }
  }
}
l
# l1 <- l
# c(l,l1)
# l3 <- c(l,l1)

#list(lhs=c("I1","I2","I5")
rules <- apriori(l, parameter= list(supp=0.1, conf=0.8, target="rules", minlen=3, maxlen=8))
inspect(rules)
summary(rules)
inspect(head(rules, by = "lift"))
plot(rules)
#plotly_arules(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

##################################################################################################################

rules <- apriori(l, parameter= list(supp=0.1, conf=0.8, target="rules", minlen=3, maxlen=8), appearance = list(lhs=c("I1","I2","I5"), default="rhs"))
inspect(rules)
summary(rules)
inspect(head(rules, by = "lift"))



library(readxl)
library(stringr)
library(arules)
library(devtools)
library(arulesViz)

packet <- read_excel("/home/weslley/Documentos/Github/dependencyHunter/APPs/hempsSYNTHETIC2_1.xlsx") 
NodeMessages <- packet[packet$Service==20, ] 
for (i in NodeMessages$Target){
  sourceNode2 <- NodeMessages[NodeMessages$Source == i, ]
  targetNode2 <- NodeMessages[NodeMessages$Target == i, ]
}

NodeMessages$TAG <- 0	

refineData <- subset(NodeMessages, !(Source != CurrentNode & CurrentNode != Target)) 
refineData <- subset(refineData, (CurrentNode != Target))

numRow<- NROW(refineData)
varTAG <- 1	

for(j in refineData$Target){
  for(i in 1:(numRow)){	
    if(refineData$TAG[i] == 0){
      if(refineData$Source[i] != j){
        refineData$TAG[i] <- paste0("I",varTAG)
      }
      
      for(y in (i+1):(numRow)){
        if((refineData$Source[y]  == refineData$Source[i])  & 
           (refineData$Service[y] == refineData$Service[i]) & 
           (refineData$Payload[y] == refineData$Payload[i]) & 
           (refineData$Target[y]  == refineData$Target[i])){
          refineData$TAG[y] <- paste0("I",varTAG)
        }
      } 
    }
    varTAG <- varTAG + 1 
  }
}

for(j in refineData$Target){
  for(i in 1:(numRow)){
    if(refineData$Source[i] == j){
      refineData$TAG[i] <- 0
    }
  }	
}

outTag <- 1

for(i in 1:(numRow)){
  if(refineData$TAG[i] == 0){
    refineData$TAG[i] <- outTag
    for(y in (i+1):(numRow)){
      if((refineData$Source[y]  == refineData$Source[i])  & 
         (refineData$Service[y] == refineData$Service[i]) & 
         (refineData$Payload[y] == refineData$Payload[i]) & 
         (refineData$Target[y]  == refineData$Target[i])){
        refineData$TAG[y] <- outTag
      }
    }
    outTag <- outTag + 1
  }
}		

G <- unlist(refineData$TAG)     #Cria uma lista com a coluna das TAGs
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

rules <- apriori(l, parameter= list(supp=0.01, conf=0.5, target="rules", minlen=3, maxlen=8))
inspect(rules)
summary(rules)
inspect(head(rules, by = "lift"))
plot(rules)


library(readxl)
library(stringr)
library(arules)
library(devtools)
library(arulesViz)

packet <- read_excel("/home/weslley/dependencyHunter/APPs/hempsSYNTHETIC2_1.xlsx") 
NodeMessages <- packet[packet$Service==20, ] 
NodeMessages$TAG <- 0	

refineData <- subset(NodeMessages, !(Source != CurrentNode & CurrentNode != Target)) 
refineData <- subset(refineData, (CurrentNode != Target))

numRow<- NROW(refineData)
varTAG <- 1	

for(j in refineData$Target){                #loop que identifica as linhas com os nós de entrada de toda a rede
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

for(j in refineData$Target){        #Correção do loop acima, tornando como 0 as linhas de saída 
  for(i in 1:(numRow)){
    if(refineData$Source[i] == j){
      refineData$TAG[i] <- 0
    }
  }	
}

outTag <- 1                  #Insere uma etiqueta em cada nó que está recebendo as mensagens, e que em seguida envia

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
  if(str_detect(G[[i]], "^[0-9]*+$")==TRUE){   #e criando um espa?o para cada sa?da correspondente. ex: out = 1, l[[1]]
    for(j in aux:i){
      rowOut <- strtoi(G[[i]], base = 0L)   #Transforma a string em numeric
      l[[rowOut]] <- G[[j]]
    }
    aux <- i 
  }                     
}                        
aux2 <- 1
for(i in 1:(g)){           #Loop auxiliar que cria listas na lista original, inserindo cada entrada na sua chave/saida 
  if(str_detect(G[[i]], "^[0-9]*+$")==TRUE){
    rowOut <- strtoi(G[[i]], base = 0L)
    while(aux2!=i+1){
      if(is.null(l[[rowOut]])==FALSE){
        l[[rowOut]] <-c(l[[rowOut]],G[[aux2]])
      }
      aux2 <- aux2 +1
    }
  }
}

# Identifica quais são os antecedentes para as regras (usado no lhs) 
L <- length(l)
input = c()
for (i in 1:L){
  X <- lengths(l[i])
  for (j in 1:X){
    if (str_detect(l[[i]][j], "([I])+[0-9]*") == TRUE){
      input <-c(input,l[[i]][j])
    }
  }
}
input <- unique(input[-1])
newList <- list()              #Cria-se uma nova lista que irá organizar os conjuntos in-out em espaços diferentes da lista base
rowNum <- 1                    #É inicializado com NA para ser possível adicionar os dados restantes com o for
newList[[rowNum]] <- NA
for (i in 1:L){          
  X <- lengths(l[i])
  for (j in 2:X){
    if (is.null(newList[[rowNum]]) == FALSE){
      newList[[rowNum]] <-c(newList[[rowNum]],l[[i]][j])
      if (str_detect(l[[i]][j], "^[0-9]*+$") == TRUE){
        rowNum <- rowNum + 1
        newList[[rowNum]] <- NA
      }
    }
  }
}


#SUPPORT => O support de um itemset X é a proporção das transações em que o X aparece. Significa a popularidade de um itemset

#CONFIDENCE => Representa a probabiliade de um item Y estar junto com um item X

#LIFT => Calcula a relação entre a confiança da regra e o suporte de um conjunto de itens na regra consequente
rules <- apriori(newList, parameter= list(supp=0.001, conf=0.8, target="rules", minlen=2),
                 appearance = list(lhs=input, default="rhs"))
# is.redundant(rules,measure = "confidence")
inspect(head(rules[is.redundant(rules)], by = "lift"))
#inspect(rules[!is.redundant(rules)])
inspect(rules)
summary(rules)
inspect(head(rules, by= "lift"))
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plotly_arules(rules[is.redundant(rules)], method = "scatterplot", measure = c("support","confidence"), shading = "lift", max = 1000)


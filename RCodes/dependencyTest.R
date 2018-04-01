library(stringr)
library(arules)

packet <- read.table("/Users/weslley/Documents/Simulações/6x6/tgff10 - 6x6.txt")
colnames(packet) <- c("Timestamp", "CurrentNode",	"Service",	"Payload", "a", "b", "Target", "c", "d", "e", "Source", "f", "g", "h")
drops <- c("a","c","d","e","f","g","h")
packet <- packet[ , !(names(packet) %in% drops)]
NodeMessages <- packet[packet$Service==20, ] 
NodeMessages <- NodeMessages[NodeMessages$b==9, ]
drops <- c("b")
NodeMessages <- NodeMessages[ , !(names(NodeMessages) %in% drops)]

tNode <- unique(NodeMessages$Target)
finalT <- data.frame()
NodeMessages$TAG <- 0
inTAG <- 1
outTAG <- 1
for (nodeTarget in tNode){
  targetNode <- NodeMessages[NodeMessages$Target == nodeTarget, ]  ### Todos as linhas com Target == nodeTarget  
  sourceNode <- NodeMessages[NodeMessages$Source == nodeTarget, ]  ### Todos as linhas com Source == nodeTarget
  mergedTable <- merge(x = targetNode, y = sourceNode, ### Uniao das duas tabelas aneriores.
                       by = c("Timestamp","CurrentNode","Service","Payload","Target","Source","TAG"), all = TRUE)
  
  refineData <- subset(mergedTable, !(Source != CurrentNode & CurrentNode != Target)) 
  refineData <- subset(refineData, (CurrentNode != Target))
  numRow<- NROW(refineData)
  
  ######### Loop para nodes apenas envia msg
  if((nodeTarget %in% refineData$Source) == TRUE & (nodeTarget %in% refineData$Target) == FALSE){
    for(i in 1:(numRow)){
      if(refineData$TAG[i] == 0){
        refineData$TAG[i] <- paste0("I",inTAG)
        if(numRow>1){
          for(y in (2):(numRow)){
            if((refineData$Source[y]  == refineData$Source[i])  &
               (refineData$Service[y] == refineData$Service[i]) &
               (refineData$Payload[y] == refineData$Payload[i]) &
               (refineData$Target[y]  == refineData$Target[i])){
              refineData$TAG[y] <- paste0("I",inTAG)
            }
          }
          inTAG <- inTAG + 1
        }
        #inTAG <- inTAG + 1
      }
    }
    x <- refineData$TAG <- 0
  }
  ######## Loop para nodes apenas recebe msg
  if((nodeTarget %in% refineData$Target) == TRUE & (nodeTarget %in% refineData$Source) == FALSE ){
    for(i in 1:(numRow)){
      if(refineData$TAG[i] == 0){
        refineData$TAG[i] <- outTAG
        if(numRow>1){
          for(y in (2):(numRow)){
            if((refineData$Source[y]  == refineData$Source[i])  &
               (refineData$Service[y] == refineData$Service[i]) &
               (refineData$Payload[y] == refineData$Payload[i]) &
               (refineData$Target[y]  == refineData$Target[i])){
              refineData$TAG[y] <- outTAG
            }
          }
          outTAG <- outTAG + 1
        }
      }
    }
    x <- refineData$TAG <- 0
  }
  ###### Loop para nodes que enviam e recebem msgs
  if((nodeTarget %in% refineData$Source) == TRUE & (nodeTarget %in% refineData$Target) == TRUE){
    for(i in 1:(numRow)){
      if(refineData$TAG[i] == 0){
        if(refineData$Source[i] != nodeTarget){
          refineData$TAG[i] <- paste0("I",inTAG)
          for(y in (2):(numRow)){
            if((refineData$Source[y]  == refineData$Source[i])  &
               (refineData$Service[y] == refineData$Service[i]) &
               (refineData$Payload[y] == refineData$Payload[i]) &
               (refineData$Target[y]  == refineData$Target[i])){
              refineData$TAG[y] <- paste0("I",inTAG)
            }
          }
          inTAG <- inTAG + 1
        }
      }
      #inTAG <- inTAG + 1
    }
    
    for(i in 1:(numRow)){
      if(refineData$TAG[i] == 0){
        refineData$TAG[i] <- outTAG
        for(y in (2):(numRow)){
          if((refineData$Source[y]  == refineData$Source[i])  &
             (refineData$Service[y] == refineData$Service[i]) &
             (refineData$Payload[y] == refineData$Payload[i]) &
             (refineData$Target[y]  == refineData$Target[i])){
            refineData$TAG[y] <- outTAG
          }
        }
        outTAG <- outTAG + 1
      }
    }
  }
  
  
  
  x <- refineData
  finalT <- rbind(finalT,x)
}

G <- unlist(finalT$TAG)   
g <- length(G) 

aux <- 1 #head(G[[1]])
l <- list()
for(i in 1:(g)){                              #La?o que percorre a lista verificando a quantidade de sa?das       
  if(str_detect(G[[i]], "^[1-9]+$")==TRUE){   #e criando um espa?o para cada sa?da correspondente. ex: out = 1, l[[1]]
    for(j in aux:i){
      rowOut <- strtoi(G[[i]], base = 0L)   #Transforma a string em numeric
      l[[rowOut]] <- G[[j]]
    }
    aux <- i 
  }                     
}                        
aux2 <- 1
for(i in 1:(g)){           #La?o auxiliar que cria listas na lista original, inserindo cada entrada na sua chave/sa?da 
  if(str_detect(G[[i]], "^[1-9]+$")==TRUE){
    rowOut <- strtoi(G[[i]], base = 0L)
    while(aux2!=i+1){
      if(is.null(l[[rowOut]])==FALSE){
        l[[rowOut]] <-c(l[[rowOut]],G[[aux2]])
      }
      aux2 <- aux2 +1
    }
  }
}
#l
# Identifica quais são os antecedentes para as regras (usado no lhs) 
L <- length(l)
input = c()
for (i in 1:L){
  X <- lengths(l[i])
  if(X>=1){
    for (j in 1:X){
      if (str_detect(l[[i]][j], "([I])+[0-9]*") == TRUE){
        input <-c(input,l[[i]][j])
      }
    }
  }
}

input <- unique(input[-1])
newList <- list()              #Cria-se uma nova lista que irá organizar os conjuntos in-out em espaços diferentes da lista base
rowNum <- 1                    #É inicializado com NA para ser possível adicionar os dados restantes com o for
newList[[rowNum]] <- NA
for (i in 1:L){          
  X <- lengths(l[i])
  if(X>=1){
    for (j in 2:X){
      if (is.null(newList[[rowNum]]) == FALSE){
        newList[[rowNum]] <-c(newList[[rowNum]],l[[i]][j])
        if(str_detect(l[[i]][j], "^[0]+$") == TRUE){
          newList[[rowNum]] <- NA
        }
        if (str_detect(l[[i]][j], "^[0-9]*+$") == TRUE){
          rowNum <- rowNum + 1
          newList[[rowNum]] <- NA
        }
      }
    }
  }
}
#newList
nTag <- unique(finalT$TAG)
nTag
tNode
tagUnique <- unique(finalT[,-1])
#write.table(tagUnique, "tag - tgff6 - 6x6.txt", sep = "\t", row.names = FALSE)

#SUPPORT => O support de um itemset X é a proporção das transações em que o X aparece. Significa a popularidade de um itemset

#CONFIDENCE => Representa a probabiliade de um item Y estar junto com um item X

#LIFT => Calcula a relação entre a confiança da regra e o suporte de um conjunto de itens na regra consequente

#classifier <- CBA(tNode ~ ., data = finalT, supp = 0.01, conf=0.9, lhs.support=TRUE)

#result <- predict(classifier, finalT)

rules <- apriori(newList, parameter= list(supp=0.001, conf=0.2, target="rules", minlen=2),
                 appearance = list(lhs=input, default="rhs"))
inspect(rules)
#rules <- eclat(newList, parameter = list(supp = 0.001, minlen = 2), control = NULL)

ec  <- eclat(newList, parameter = list(support = 0.001))
rec <- ruleInduction(ec, confidence = 0.2)
inspect(ec)
rec
inspect(rec)
inspect(subset( rec, subset = lhs %pin% input) )

# is.redundant(rules,measure = "confidence")
#inspect(head(rules[is.redundant(rules)], by = "lift"))
#inspect(rules[is.redundant(rules)])

summary(rules)
inspect(head(rules, by= "lift"))
image(tidLists(rec))
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plotly_arules(rules[is.redundant(rules)], method = "scatterplot", measure = c("support","confidence"), shading = "lift", max = 1000)



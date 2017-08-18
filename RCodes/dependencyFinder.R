library(readxl)
library(stringr)
library(arules)
library(devtools)
library(arulesViz)

packet <- read_excel("/home/weslley/Documentos/Github/dependencyHunter/APPs/hempsSYNTHETIC2.xlsx") 
NodeMessages <- packet[packet$Service==20, ] 
# for (i in NodeMessages$Target){
#   sourceNode2 <- NodeMessages[NodeMessages$Source == i, ]
#   targetNode2 <- NodeMessages[NodeMessages$Target == i, ]
# }

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
  if(str_detect(G[[i]], "^[0-9]*+$")==TRUE){   #e criando um espa?o para cada sa?da correspondente. ex: out = 1, l[[1]]
    for(j in aux:i){
      rowOut <- strtoi(G[[i]], base = 0L)   #Transforma a string em numeric
      l[[rowOut]] <- G[[j]]
    }
    aux <- i 
  }                     
}                        
aux2 <- 1
for(i in 1:(g)){           #Laço auxiliar que cria listas na lista original, inserindo cada entrada na sua chave/sa?da 
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

# Laço que identifica quais são os antecedentes para as regras 
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
newList <- list()
rowNum <- 1
newList[[rowNum]] <- 0
for (i in 1:L){
  X <- lengths(l[i])
  for (j in 2:X){
    if (is.null(newList[[rowNum]]) == FALSE){
      newList[[rowNum]] <-c(newList[[rowNum]],l[[i]][j])
      if (str_detect(l[[i]][j], "^[0-9]*+$") == TRUE){
        rowNum <- rowNum + 1
        newList[[rowNum]] <- 0
      }
    }
  }
}

newList

rules <- apriori(newList, parameter= list(supp=0.01, conf=0.5, target="rules", minlen=2),
                 appearance = list(lhs=input, default="rhs"))
# is.redundant(rules,measure = "confidence")
inspect(head(rules[is.redundant(rules)], by = "lift"))
#inspect(rules[!is.redundant(rules)])
inspect(rules)
summary(rules)
inspect(head(rules, by = "lift"))
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plotly_arules(rules[is.redundant(rules)], method = "scatterplot", measure = c("support","confidence"), shading = "lift", max = 1000)


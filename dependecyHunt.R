library(readxl)

packet <- read_excel("/home/raphael/Downloads/hempsMPEG.xlsx") ### Local onde fica o arquivo com os dados da simulação.

nodeTarget  <- 2 ###scan()


targetNode <- packet[packet$Target == nodeTarget, ]  ### Todos os pacotes com Target == nodeTarget  
sourceNode <- packet[packet$Source == nodeTarget, ]  ### Todos os pacotes com Source == nodeTarget
mergedTable <- merge(x = targetNode, y = sourceNode, ### União das duas tabelas aneriores.
                     by = c("Timestamp","Source","CurrentNode","Service","Payload","Target"), all = TRUE)

refinedData <- subset(mergedTable, !(Source != CurrentNode & CurrentNode != Target)) ### Mostra somente as linhas com o pacote a ser enviado, ou pacotes recebidos.
UniqueValues <- unique(refinedData[,-1])											 ### 




##UniqueValues <- refinedData ##UniqueValues
##UniqueValues$CurrentNode <- NULL
UniqueValues$tag <- 0
numRow<- NROW(UniqueValues)
varTag <- 1
for(i in 1:(numRow-1)){
  if(UniqueValues$tag[i] == 0){
    UniqueValues$tag[i] <- paste0("P",varTag)
    for(y in (i+1):(numRow)){
      if((UniqueValues$Source[y]  == UniqueValues$Source[i])  & 
         (UniqueValues$Service[y] == UniqueValues$Service[i]) & 
         (UniqueValues$Payload[y] == UniqueValues$Payload[i]) & 
         (UniqueValues$Target[y]  == UniqueValues$Target[i])){
        UniqueValues$tag[y] <- paste0("P",varTag)
        
      }else{
        print("Já está tagueada")
      }
    }
    varTag <- varTag - 1
  }
  varTag <- varTag + 1
}







######################################FUNÇÃO SEM AÇÃO POR ENQUANTO#####################################################################################

a <- as.vector(t(sourceNode$Header))                ### Transpõe os valores da coluna Header no nó 1
b <- as.vector(t(targetNode$Header))                ### Transpõe os valores da coluna Header no nó 2
###nodeTarget <- packet[packet$Target == 0, ]    ### Linhas com target igual ao segundo nó
MACROA <- length(a)
MACROB <- length(b)

### Valor MACROdependecy com tamanho do vetor de dependecy, deve ter tamanho igual ao número de pacotes enviados pelo segundo nó.
if (MACROB < MACROA){
  MACROdependecy <- MACROB
} else{
  MACROdependecy <- MACROA
}


dependecy <-rep(0,MACROdependecy)     ### Popula o vetor dependecy com zeros




### Laço que captura TODAS as possíveis dependencias entre dois nós, sem filtragem.
for(i in 1:MACROB){                               ### Números de pacotes do nó B
  for(j in 1:MACROA){                             ### Números de pacotes do nó B
    if(a[j] < b[i]){                              ### Vê todos os pacotes do nó A que foram enviados antes do nó B enviar os pacotes
      ### Colocar pacote que originou a dependencia
      dependecy[i] <- dependecy[i] + 1            ### Calcula o número de possiveis dependecias entre cada pacote enviado pelo nó B
      
    }
  }
}

dependecy
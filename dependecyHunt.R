library(readxl)
packet <- read_excel("/home/raphael/Downloads/hempsMPEG.xlsx") ### Local onde fica o arquivo com os dados da simulação.

nodeTarget  <- 257 ###scan()


targetNode <- packet[packet$Target == nodeTarget, ]  ### Todos os pacotes com Target == nodeTarget  
sourceNode <- packet[packet$Source == nodeTarget, ]  ### Todos os pacotes com Source == nodeTarget
mergedTable <- merge(x = targetNode, y = sourceNode, by = c("Timestamp","Source", "CurrentNode","Service","Payload","Target"), all = TRUE)
##write.table(final, file = "pacotes.txt",sep = "\t",eol = "\n", na = "-",row.names = FALSE)
refinedData <- subset(mergedTable, !(Source != CurrentNode & CurrentNode != Target)) ##Mostra somente as linhas com o pacote a ser enviado, ou pacotes recebidos
UniqueValues <- unique(refinedData[,-1])




UniqueValues2 <- UniqueValues
UniqueValues2$CurrentNode <- NULL
UniqueValues2$tag <- 0
numRow<- NROW(UniqueValues2)

##for(y in 1:b){
##  for(i in (y+1):b-1){
##    if((UniqueValues2$Source[y] == UniqueValues2$Source[i]) & (UniqueValues2$Service[y] == UniqueValues2$Service[i]) & (UniqueValues2$Payload[y] == UniqueValues2$Payload[i]) & (UniqueValues2$Target[y] == UniqueValues2$Target[i])){
##      print("Mesmo pacote")
##    }
##  }
##}


###Função que tagueia os pacotes
for(i in 1:numRow){
  if(UniqueValues2$tag[i] == 0){
    UniqueValues2$tag[i] <- i
    for(y in (i+1):(numRow-1)){
      if((UniqueValues2$Source[y] == UniqueValues2$Source[i]) & (UniqueValues2$Service[y] == UniqueValues2$Service[i]) & 
        (UniqueValues2$Payload[y] == UniqueValues2$Payload[i]) & (UniqueValues2$Target[y] == UniqueValues2$Target[i])){
         UniqueValues2$tag[y] <- i
    
  }else{
    print("Já está tagueada")
  }
}
  
}
}






a <- as.vector(t(sourceNode$Timestamp))                ### Transpõe os valores da coluna Header no nó 1
b <- as.vector(t(targetNode$Timestamp))                ### Transpõe os valores da coluna Header no nó 2
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


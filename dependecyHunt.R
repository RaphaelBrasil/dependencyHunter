library(readxl)
<<<<<<< HEAD
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

=======
library(hashmap)
packet <- read_excel("/home/weslley/Downloads/hempsMPEG.xlsx") ### Local onde fica o arquivo com os dados da simulaÃ§Ã£o.

nodeTarget  <- 257 ###scan()

>>>>>>> e294b6c56f5ab09c977c6f9fd64debbbe2d263a9

targetNode <- packet[packet$Target == nodeTarget, ]  ### Todos os pacotes com Target == nodeTarget  
sourceNode <- packet[packet$Source == nodeTarget, ]  ### Todos os pacotes com Source == nodeTarget
mergedTable <- merge(x = targetNode, y = sourceNode, 
               by = c("Header","Source", "Address","Service","Payload","Target"), all = TRUE)
##write.table(final, file = "pacotes.txt",sep = "\t",eol = "\n", na = "-",row.names = FALSE)
refinedData <- subset(final, !(Source != Address & Address != Target)) ##Mostra somente as linhas com o pacote a ser enviado, ou pacotes recebidos
UniqueValues <- unique(final[,-1])




<<<<<<< HEAD
a <- as.vector(t(sourceNode$Timestamp))                ### Transpõe os valores da coluna Header no nó 1
b <- as.vector(t(targetNode$Timestamp))                ### Transpõe os valores da coluna Header no nó 2
###nodeTarget <- packet[packet$Target == 0, ]    ### Linhas com target igual ao segundo nó
=======








a <- as.vector(t(sourceNode$Header))                ### TranspÃµe os valores da coluna Header no nÃ³ 1
b <- as.vector(t(targetNode$Header))                ### TranspÃµe os valores da coluna Header no nÃ³ 2
###nodeTarget <- packet[packet$Target == 0, ]    ### Linhas com target igual ao segundo nÃ³
>>>>>>> e294b6c56f5ab09c977c6f9fd64debbbe2d263a9
MACROA <- length(a)
MACROB <- length(b)

### Valor MACROdependecy com tamanho do vetor de dependecy, deve ter tamanho igual ao nÃºmero de pacotes enviados pelo segundo nÃ³.
if (MACROB < MACROA){
  MACROdependecy <- MACROB
} else{
  MACROdependecy <- MACROA
}


dependecy <-rep(0,MACROdependecy)     ### Popula o vetor dependecy com zeros




### LaÃ§o que captura TODAS as possÃ­veis dependencias entre dois nÃ³s, sem filtragem.
for(i in 1:MACROB){                               ### NÃºmeros de pacotes do nÃ³ B
  for(j in 1:MACROA){                             ### NÃºmeros de pacotes do nÃ³ B
    if(a[j] < b[i]){                              ### VÃª todos os pacotes do nÃ³ A que foram enviados antes do nÃ³ B enviar os pacotes
      ### Colocar pacote que originou a dependencia
      dependecy[i] <- dependecy[i] + 1            ### Calcula o nÃºmero de possiveis dependecias entre cada pacote enviado pelo nÃ³ B
      
    }
  }
}

<<<<<<< HEAD
dependecy

=======
dependecy
>>>>>>> e294b6c56f5ab09c977c6f9fd64debbbe2d263a9

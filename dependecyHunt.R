library(readxl)
library(hashmap)
packet <- read_excel("/home/weslley/Downloads/hempsMPEG.xlsx") ### Local onde fica o arquivo com os dados da simulação.

nodeTarget  <- 1 ###scan()


targetNode <- packet[packet$Target == nodeTarget, ]  ### Todos os pacotes com Target == nodeTarget  
sourceNode <- packet[packet$Source == nodeTarget, ]  ### Todos os pacotes com Source == nodeTarget
final <- merge(x = targetNode, y = sourceNode, by = "Header", all = TRUE)
write.table(final, file = "pacotes.txt",sep = "\t",eol = "\n", na = "-",row.names = FALSE)

UniqueValues <- unique(final[,-1])


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
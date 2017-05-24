library(readxl)
packet <- read_excel("/home/weslley/Downloads/hempsMPEG.xlsx") ### Local onde fica o arquivo com os dados da simulação.

noDestino  <- 1 ###scan()


node <- packet[packet$Target == noDestino, ]  ### Todos os pacotes com Target == noDestino  
noor <- packet[packet$Source == noDestino, ]  ### Todos os pacotes com Source == noDestino
final <- merge(x = node, y = noor, by = "Header", all = TRUE)


a <- as.vector(t(noor$Header))                ### Transpõe os valores da coluna Header no nó 1
b <- as.vector(t(node$Header))                ### Transpõe os valores da coluna Header no nó 2
###noDestino <- packet[packet$Target == 0, ]    ### Linhas com target igual ao segundo nó
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
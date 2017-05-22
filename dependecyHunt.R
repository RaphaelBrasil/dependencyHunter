library(readxl)
packet <- read_excel("C:/Users/Rogério/Documents/R/Pacotes Hemps MPEG.xlsx") ### Local onde fica o arquivo com os dados da simulação.


no0   <-packet[packet$Source == 0, ]    ### Linhas com source igual a 0
no1   <-packet[packet$Source == 1, ]    ### Linhas com source igual a 1
no2   <-packet[packet$Source == 2, ]    ### Linhas com source igual a 2
no256 <-packet[packet$Source == 256, ]  ### Linhas com source igual a 256
no257 <-packet[packet$Source == 257, ]  ### Linhas com source igual a 257
no258 <-packet[packet$Source == 258, ]  ### Linhas com source igual a 258
no512 <-packet[packet$Source == 512, ]  ### Linhas com source igual a 512
no513 <-packet[packet$Source == 512, ]  ### Linhas com source igual a 513
no514 <-packet[packet$Source == 512, ]  ### Linhas com source igual a 514

a <- as.vector(t(no1$Header))             ### Transpõe os valores da coluna Header no nó 1
b <- as.vector(t(no512$Header))           ### Transpõe os valores da coluna Header no nó 2
MACROA <- length(a)
MACROB <- length(b)

### Valor MACROdependecy com tamanho do vetor de dependecy, deve ter tamanho igual ao número de pacotes enviados pelo segundo nó.
if (MACROB > MACROA){
  MACROdependecy <- MACROB
} else{
  MACROdependecy <- MACROA
}


dependecy <-rep(0,MACROdependecy)     ### Popula o vetor dependecy com zeros




### Laço que captura TODAS as possíveis dependencias entre dois nós, sem filtragem.
for(i in 1:MACROB){                               ### Números de pacotes do nó B
  for(j in 1:MACROA){                             ### Números de pacotes do nó B
    if(a[j] < b[i]){                              ### Vê todos os pacotes do nó A que foram enviados antes do nó B enviar os pacotes
      dependecy[i] <- dependecy[i] + 1            ### Calcula o número de possiveis dependecias entre cada pacote enviado pelo nó B
      
    }
  }
}

dependecy


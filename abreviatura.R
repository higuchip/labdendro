#--------------------------------------------------------------------------------------------
# Abreviando o nome científico de espécies usando a função make.cepnames da biblioteca vegan
#
# Pedro Higuchi
# Departamento de Engenharia Florestal
# Universidade do Estado de Santa Catarina
# Contato: higuchip [at] gmail.com
#
#---------------------------------------------------------------------------------------------

teste<-read.csv("http://dl.dropbox.com/u/6511995/testeR.csv", header=T, row.names=1, sep=";") #para importar uma matriz com dados hipotéticos
teste # o resultado deve ser uma matriz 5x5, sendo que as linhas correspondem às espécies e as colunas indicam as parcelas
library(vegan) #a biblioteca vegan deve estar previamente instalada
colnames(teste) <- make.cepnames(colnames(teste))
teste #observe os nomes das espécies abreviados

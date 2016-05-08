#------------------------------------------------------------------------------------------
# Curva de acumulação de espécies no R, por meio da função specaccum, da biblioteca vegan
# Pedro Higuchi
# Departamento de Engenharia Florestal
# Universidade do Estado de Santa Catarina
# Contato: higuchip [at] gmail.com
#------------------------------------------------------------------------------------------

teste<-read.csv("http://dl.dropbox.com/u/6511995/testeR.csv", header=T, row.names=1, sep=";") #para importar uma matriz com dados hipotéticos
teste # o resultado deve ser uma matriz 7x10, sendo que as linhas correspondem às espécies e as colunas indicam as parcelas
library(vegan) #a biblioteca vegan deve estar previamente instalada
?specaccum #obter ajudar a respeito da função
#Curva de acumulacao de especies 
acum1 <- specaccum(teste, permutations = 1000) # encontra o número de espécie esperado de acordo com os autores listados em #Referências 
acum2 <- specaccum(teste, "random", permutations = 1000) # encontra a média do número de espécie e o desvio padrão a partir de 1000 permutações 
plot(acum1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="darkgray", las=1, ylab="Número de espécies", xlab="Número de parcelas", font=5, font.lab=6, cex.lab=2, cex.axis=1.5) 
boxplot(acum2, col="lightgray", add=TRUE, pch="+")
 
#Referências
#Ugland, K.I., Gray, J.S. & Ellingsen, K.E. (2003). The species-accumulation curve and estimation
#of species richness. Journal of Animal Ecology 72: 888–897.
#Colwell, R.K., Mao, C.X. & Chang, J. (2004). Interpolating, extrapolating, and comparing incidencebased species accumulation curves. Ecology 85: 2717–2727.
#Kindt R., Van Damme, P. & Simons, A.J. (2006) Patterns of species richness at varying scales in
#western Kenya: planning for agroecosystem diversiﬁcation. Biodiversity and Conservation, online
#ﬁrst: DOI 10.1007/s10531-005-0311-9

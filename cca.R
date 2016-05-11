#------------------------------------------------------------------------------------------
# Rotina para realizar CCA, por meio da da biblioteca vegan
# Pedro Higuchi
# Departamento de Engenharia Florestal
# Universidade do Estado de Santa Catarina
# Contato: higuchip [at] gmail.com
#------------------------------------------------------------------------------------------

#Carregando as matrizes do exemplo

library(vegan)
data(varespec) # matriz com dados vegetacionais, com  24 linhas (parcelas) e 44 colunas (espécies). 
data(varechem) # matriz com dados ambientais, com 24 linhas (parcelas) e 14 colunas (variáveis), referente às características edáficas de cada parcela
varespec # visualizar matriz
varechem # visualizar matriz

#Havendo necessidade da remoção de espécies raras:
#somaindsp<- apply(matrizvegetacional, 2, sum)
#semrararas<- matrizvegetacional[, somaindsp>10]
#Onde:
#semraras: nova matriz criada, sem a presença de espécies com menos do que 10 indivíduos;
#matrizvegetacional: nome do objeto que contem a matriz vegetacional completa.

#Padronização log + 1, para corrigir possível assimetria da distribuição dos dados

varespecslog <- decostand(varespec, "log")

#Seleção das variáveis ambientais por stepwise, utilizando testes de permutação

fullmodel<-cca(varespecslog ~ .,varechem)
smallmodel<-cca(varespecslog ~ 1, varechem)
fit_model<- ordistep(smallmodel, scope=formula(fullmodel))
fit_model # visualizar resultados
summary(fit_model) # sumário dos resultados

#Calcular fator de inflação (VIF), para excluir variáveis multicolineares

vif.cca(fit_model)
fit_model
fit_model<- cca(varespecslog ~ Mn, data=varechem) # refazendo cca sem variáveis com VIF > 10
fit_model # visualizar resultados
summary(fit_model) # sumário dos resultados

#De acordo com Oksanen (2011), valores de VIF = 1 indicam variáveis completamente independentes entre si.
#Valores de VIF = 10 a 20 indicam variáveis aumentente correlacionadas entre si.

#Teste de permutação para avaliar significâncias da CCA, variáveis e eixos

anova.cca(fit_model)
anova.cca(fit_model, by="terms")
anova.cca(fit_model, by="axis")

#Apresentação gráfica

plot(fit_model, type="n", las=1, font=6, font.axis=6, font.lab=6, cex.lab=1.5, cex.lab=1.5)
stems <- colSums(varespec)
orditorp(fit_model, "sp", priority=stems, pch="+", pcol="grey", font=8, cex=1)
text(fit_model, dis="cn", font=6, cex=1.5)
text(fit_model, dis="sites", cex=1, font=7)

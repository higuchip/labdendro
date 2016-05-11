#-------------------------------------------------------------------------------------------------------------------------
# Rotina para avaliar a influência da resolução filogenética sobre NRI. Baseado nos trabalhos de:
# * Swenson, N. G. (2009). Phylogenetic resolution and quantifying the phylogenetic diversity and dispersion of communities. 
#   PloS one, 4(2), e4390.
# * Gastauer, M., Saporetti‐Junior, A. W., Magnago, L. F. S., Cavender‐Bares, J., & Meira‐Neto, J. A. A. (2015). 
#   The hypothesis of sympatric speciation as the dominant generator of  endemism in a global hotspot of biodiversity.
#   Ecology and Evolution, 5(22), 5272-5283.
# Pedro Higuchi
# Departamento de Engenharia Florestal
# Universidade do Estado de Santa Catarina
# Contato: higuchip [at] gmail.com
# Observação: Caso tenha sido útil em seu trabalho, favor citá-la
#-------------------------------------------------------------------------------------------------------------------------

library(picante)

# Simulando 10.000 árvores filogenéticas resolvidas, sem politomias, por meio da função multi2di, para cada uma das três áreas avaliadas

foo<- as.list(rep("", 10000)) 
for (i in 1:10000){
  foo[[i]]<-(multi2di(phy))  #phy - árvore filogenética datada por meio do phylocom
} 

# Calculando métricas filogenéticas a partir das árvores sem politomias
foo_1<- as.list(rep("", 10000)) 
for (i in 1:10000){
  foo_1[[i]]<-ses.mpd(comm, cophenetic(foo[[i]]),null.model="independentswap",  
                    abundance.weighted=T)  ##comm - dados de comunidade sites x spp
}

# Criando uma tabela com as métricas filogenéticas sem politomias

filo_resolvida<-do.call(rbind, foo_1)

# Calculando métricas filogenéticas da árvore com politomias

foo_2<- as.list(rep("", 10000)) 
foo_2
for (i in 1:10000) 
  {
  foo_2[[i]]<-ses.mpd(comm, cophenetic(phy),null.model="independentswap",  
                      abundance.weighted=T)
  }

#Criando uma tabela com as métricas filogenéticas sem politomias

filo_politomia<-do.call(rbind, foo_2)


#Verificando o padrão de dispersão dos valores de NRI para árvores sem politomias x árvores com politomias

plot(filo_resolvida[1:10000,6],filo_politomia[1:10000,6],pch=20, cex=.4,
     ylim=c(-2,2), xlim=c(-2,2) )


# Fazendo o subset para as áreas avaliadas (1, 2, 3)

filo_resolvida_area1<-subset(filo_resolvida, filo_resolvida$mpd.obs.z < 0 & filo_resolvida$mpd.obs.z > -1)
filo_poli_area1<-subset(filo_politomia, filo_politomia$mpd.obs.z > -1 & filo_politomia$mpd.obs.z < 0)

filo_resolvida_area2<-subset(filo_resolvida, filo_resolvida$mpd.obs.z < -1)
filo_poli_area2<-subset(filo_politomia, filo_politomia$mpd.obs.z < -1)

filo_resolvida_area3<-subset(filo_resolvida, filo_resolvida$mpd.obs.z > 0)
filo_poli_area3<-subset(filo_politomia, filo_politomia$mpd.obs.z >0)


#Ajustando regressão linear forçando o ajuste passar pela origem

#Area 1

plot(filo_resolvida_area1[1:10000,6],filo_poli_area1[1:10000,6],pch=20, cex=.4, ylim=c(-0.5,0.5), xlim=c(-0.5,0.5))
lm.resol.area1 <- lm(filo_resolvida_area1[1:10000,6] ~  filo_poli_area1[1:10000,6]-1)

#Verificando o coeficiente angular e R2

summary(lm.resol.area1)

#Valores de R2 indicam a correlação entre árvores com e sem politomias
#Se coeficiente angular > 1, então tendência de falso positivo (erro tipo I)
#Se coeficiente angular < 1, então tendência de falso negativo (erro tipo II)

abline(lm.resol.area1,lty=1,lwd=2)
abline(h=0,v=0,col=8)
abline(0,1, lty=2, lwd=3)

#Área 2
plot(filo_resolvida_area2[1:10000,6],filo_poli_area2[1:10000,6],pch=20, cex=.4, ylim=c(-2,.5), xlim=c(-2,.5) )
lm.resol.area2 <- lm(filo_resolvida_area2[1:10000,6] ~  filo_poli_area2[1:10000,6]-1)
lm.resol.area2
summary(lm.resol.area2)
abline(lm.resol.area2,lty=1,lwd=1)
abline(h=0,v=0,col=8)
abline(0,1, lty=2, lwd=2)


#Área 3
plot(filo_resolvida_area3[1:10000,6],filo_poli_area3[1:10000,6],pch=20, cex=.4,  ylim=c(-0.5,1), xlim=c(-0.5,1) )
lm.resol.area3 <- lm(filo_resolvida_area3[1:10000,6] ~  filo_poli_area3[1:10000,6]-1)
lm.resol.area3
summary(lm.resol.area3)
abline(lm.resol.area3,lty=1,lwd=1)
abline(h=0,v=0,col=8)
abline(0,1, lty=2, lwd=2)

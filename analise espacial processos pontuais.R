
#ANÁLISE DE PROCESSOS PONTUAIS ESPACIAIS


#O que é um processo pontual?

# --> Eventos que ocorrem em um espaço, que podem ser representados 
#na forma de pontos

#Exemplos: arvores em uma floresta, diferentes indivíduos de espécies
#arbóreas, árvores mortas, árvores recrutadas.

#Objetivo: a investigação de padrões espacias de segunda ordem.

#Primeira ordem x Segunda ordem.



install.packages("spatstat")

library(spatstat)

# Coordenadas espacias
coord<-read.table("xylopia_brasiliensis_spatial_distribution.csv",
                  header=T, sep=";", dec=",")
head(coord)
#O poligono
poli<-read.table("polygon_boundary.txt",
                 header=T, sep=" ", dec=".")

head(poli)


#Define o poligono da  área estudada
w <- owin(poly=list(x=poli$X1,y=poli$X2)) 
w

#Definir coordenadas espaciais no formato específico do spatstat
coord.pts<-ppp(coord$x,coord$y, window=w)
coord.pts


plot(coord.pts)

plot(density(coord.pts))


plot(w, add=T)
points(coord.pts, pch=20, cex=.5, col="black")

plot(density(coord.pts), main="", 
     col=gray.colors(50))
plot(w, add=T)
points(coord.pts, pch=20, cex=.5, col="black")



norm_palette <- colorRampPalette(c("white","black"))
pal_trans <- norm_palette(3)

plot(density(coord.pts), main="", col=pal_trans)
plot(w, add=T)
points(coord.pts, pch=20, cex=.5, col="black")



###Testar a hipotese nula de que 
#o processo pontual é homogeneo - sem tendencia espacial de primeira ordem
#to test the null hypothesis that the point
#process is a homogeneous Poisson process, against the alternative that it is
#an inhomogeneous Poisson process with intensity of the form


#Distribuicao completamente homogenea na area
plot(coord.pts)
fit.coord.pts.0 <- ppm(coord.pts~1, Poisson())

#Poisson process with intensity 
#loglinear in x coordinate

fit.coord.pts.1 <- ppm(coord.pts~x, Poisson()) 


anova.ppm(fit.coord.pts.0, 
           fit.coord.pts.1, test="Chisq")


mod0 <- ppm(swedishpines ~1)
modx <- ppm(swedishpines ~x)
anova(mod0, modx, test="Chi")
##FUNCAO K DE RIPLEY

#Se não existir tendência espacial de primeira ordem,
#usar Lest.
#Se existir tendência espacial de primeira ordem, 
#usar Linhom

EL.inhom <- envelope(coord.pts, 
                     Linhom, nsim=99, 
                     correction="best")

plot(EL.inhom, . - r ~ r, 
     ylab=expression(hat("L")), legend=F,
     xlab="Distância (m)")



EL.hom <- envelope(coord.pts, Lest, nsim=99, correction="best")
plot(EL.hom, . - r ~ r, ylab=expression(hat("L")), legend=F,
     xlab="Distância (m)")



###PAIR CORRELATION FUNCTION

###Semelhante ao O-ring statistics

Epcf.inhom <- envelope(coord.pts, pcfinhom,
                       nsim=99, correction="best")
plot(Epcf.inhom, ylab="PCF", legend=F,
     xlab="Distância (m)")


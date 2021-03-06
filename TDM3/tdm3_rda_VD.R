setwd("C:\\Users\\Xavier\\FORMANRISK\\cours\\tdm3")


bdd<-read.csv("data_Allard.csv", header=T,sep=";",dec=",")
str(bdd)
names(bdd)
# Pr�parer les 5 types de base
spp<-bdd[,-c(1:23)];str(spp)#391 spp, beaucoup d'spp pour nbre de lignes
geog<-bdd[,4:6];str(geog)
sol<-bdd[,7:21];str(sol)

gestion<-bdd[,c(22:23)];str(gestion)

library(vegan)
div<-data.frame(Ab=apply(spp,1,sum),RS=specnumber(spp),Sha=diversity(spp,index="shannon"));str(div)

# Representation geographique
library(maps);library(mapdata)
map(database="worldHires",xlim=range(geog$Latitude),ylim=range(geog$Longitude),mar=c(0,0,0,0), resolution=1)
points(geog$Latitude,geog$Longitude, bg=c(1:9)[bdd$Pays],pch=c(21:24,21:24)[bdd$Pays])
levels(bdd$Pays)

#Comment simplifier le nombre d'esp�ce: methode du tri par abondances
library(pastecs)
spp.abd <- abund(spp, f=0.2)  #Application de la m�thode TPA
plot(spp.abd, dpos=c(40,100),xlab="Genres",ylab= "Abondances relatives", main="M�thode TPA f=0.2" ) #Repr�sentation graphique
spp.abd$n <- identify(spp.abd) # Un petit plateau est visible, cliquer sur la fin du plateau
#Number of variables extracted: 85 on a total of 391 
spp2 <- extract(spp.abd, spp.abd$n) # Nouvelle base de donn�es avec 83 
str(spp2)# On n'a beaucoup moins d'spp et on pourra faire une analyse factorielle spp<relev�s

#=>Objectif: expliquer une structure avec des variables qui conraignent pour meilleure quantification

#Expliquer sol par geographie=>ACP sous contraintes
###################################################
library(vegan)
str(sol);str(geog)
soltrf<-as.data.frame(scale(sol))
rda <- rda(soltrf ~ ., geog)# Le modele global
rda

ordiplot(rda)

#Choix des meilleures variables explicatives
?ordistep
step.forward<-ordistep(rda(soltrf ~ 1, geog),scope = formula(rda), direction="both",pstep=1000)     
#Ici toutes les variables sont gard�es
rda2 <- rda(soltrf ~ Altitude+Longitude+Latitude, geog)  
R2<-RsquareAdj(rda2)$r.squared;R2 #Calcul de l'inertie expliqu�e par le mod�le final
anova.cca(rda2, step=1000) # Significativit� du mod�le global
anova.cca(rda2,by="axis",step=1000) # Les axes significatifs: 3

rda2 #Visualisation globale des r�sultats. Ne sont donn�s ici que les r�sultats n�cessaires pour calculer le pourcentage expliqu� par les axes, soit les valeurs propres contraintes accumul�es
2.57/15#17% expliqu� par axe 1: L'essentiel ici!!! (constrained)
0.3574/15#2% expliqu� par axe 2
0.1652/15#1% expliqu� par axe 3

(2.57+0.3574+0.1652)/15
plot(rda2)

scores(rda2,display="species",scaling=1)->Param# scaling 1 pour relation entre var expliqu�es
scores(rda2,display="sites",scaling=2)->Site# scaling 2 pour relation entre sites

#Repr�sentation graphique: sur 2 permiers axes suffit
x11(); par(mfrow=c(2,2)) ;
plot(rda2, scaling=1)# pour relation entre var expliqu�es
text(Param,labels=rownames(Param),col="red") 

plot(rda2, scaling=2) # pour relation entre sites
s.class(Site,fac=bdd$Pays) # Repr�sentation de la dispersion de ces �l�ments par pays

#Expliquer communaut�s par sol et geographie
#=>AFC sous contraintes
###################################################
library(vegan)
detach("package:ade4", unload=TRUE)
str(spp2);
exp<-data.frame(sol,geog);str(exp)
spptrf<-as.data.frame(log1p(spp2))

cca <- cca(spptrf ~ ., exp)# Le modele global

#Choix des meilleures variables explicatives
step.forward<-ordistep(cca(spptrf ~ 1, exp),scope = formula(cca), direction="both",pstep=1000)     
#Ici toutes les variables ne sont pas gard�es
cca2 <- cca(spptrf ~ Latitude + Longitude + pH + Altitude + NOP + Prof_sol +Hauteur_vegetation + NO3 + Mn + Zn , exp)  
R2<-RsquareAdj(cca2)$r.squared;R2 #Calcul de l'inertie expliqu�e par le mod�le final
anova.cca(cca2, step=1000) # Significativit� du mod�le global
anova.cca(cca2,by="axis",step=1000) # Les axes significatifs: 8
cca2 #Visualisation globale des r�sultats. Ne sont donn�s ici que les r�sultats n�cessaires pour calculer le pourcentage expliqu� par les axes, soit les valeurs propres contraintes accumul�es

0.16551/2.0580#8% expliqu� par axe 1: L'essentiel ici!!! (constrained)
0.11127/2.0580#6% expliqu� par axe 2
0.06495/2.0580 #...plus grand chose
scores(cca2,display="species",scaling=1)->Sp# scaling 1 pour relation entre var expliqu�es
scores(cca2,display="sites",scaling=2)->Site# scaling 2 pour relation entre sites

#Repr�sentation graphique: sur 2 premiers axes suffit
x11(); par(mfrow=c(2,2)) ;
plot(cca2, scaling=1)# pour relation entre var expliqu�es
text(Sp,labels=rownames(Sp),col="red",cex=0.5)        
plot(cca2, scaling=2) # pour relation entre sites
library(ade4);s.class(Site,fac=bdd$Pays) # Repr�sentation de la dispersion de ces �l�ments par pays


### Fiche Analyse des Correspondances Multiples


## Comprendre l'organisation des données

#Générer un tableau de données factices
NINDIV = 5 # changer la veleur pour modifier la taille du tableau (le nombre d'individus statistiques = nombre de lignes)

EXEMPLE<-data.frame(
Var1 = sample(1:3, NINDIV, replace = T), 
Var2 = sample(1:3, NINDIV,replace = T),
Var3 =sample(1:3, NINDIV,replace = T))

str(EXEMPLE) # Afficher la structure du tableau, noter que les variables sont pour le moment considérées comme des nombres entiers (int = integer)
head(EXEMPLE)
summary(EXEMPLE)
rownames(EXEMPLE)<-paste0("ind", 1:NINDIV)

# transformer les variables en variables qualitatives (facteurs)
for(i in 1:ncol(EXEMPLE)){as.factor(EXEMPLE[,i])->EXEMPLE[,i]} 
str(EXEMPLE)
head(EXEMPLE) # Les varaibles sont maintenant des facteurs
summary(EXEMPLE)

#visualiser les histogrammes de répartiton de chaque modalités pour chacune des variables

par(mfrow = c(2,2)) # Afficher simultanément plusieurs graphiques

for(i in 1:3){ # 3 = nombres de variables (colonnes) dans le tableau 
plot(EXEMPLE[,i], main = colnames(EXEMPLE)[i])
}


# Afficher le tableau de Burt et le tableau disjonctif complete
library(ade4)
acm.disjonctif(EXEMPLE)
acm.burt(EXEMPLE, EXEMPLE)
acm.disjonctif(EXEMPLE)

## Organisation des données
library(ade4)
data(ours)
?ours
str(ours)
head(ours)

#visualiser les histogrammes de répartiton de chaque modalités pour chacune des variables
par(mfrow = c(3,3))
for(i in 1:8){plot(ours[,i], main = colnames(ours)[i])}

tiff("alti.tif")
plot(ours[,1], main = colnames(ours)[1], cex.main = 2, cex.axis=2, cex =2)
dev.off()

# Réalisation de l'ACM

acm1 <- dudi.acm(ours[,1:8])

# Inertie associée et contributions relatives aux axes (cos²)
inertia.dudi(acm1, row.inertia =TRUE , col.inertia =TRUE)

# Représentation graphique
tiff("ACM.tif", w =600, h =600, points = 20)
scatter(acm1)
dev.off()

# Corrélations entre variables
acm1$cr

#Visualisation graphique
par(mfrow = c(1, 2), las = 2)
barplot(acm1$cr[, 1], ylim = c(0,1),names.arg = rownames(acm1$cr) , 
main = "Axe 1")
barplot(acm1$cr[, 2],ylim = c(0,1), names.arg = rownames(acm1$cr),
main = "Axe 2")

# Inertie associée et cos²
inertia.dudi(acm1, col.inertia = TRUE, row.inertia =TRUE )


#Projeter les citations de l'ours avant sa disparition
tiff("var_sup.tif", w =600, h =600, points = 30)
s.label(acm1$li, clabel= 0,pch = 20) # Projeter le plan factoriel
s.class(acm1$li, ours[,9], add.plot = T) 
dev.off()
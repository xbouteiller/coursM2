# Xavier Bouteiller

# Ce  cours est basé sur les fiches disponibles sur :
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/



# ------------------------------------------------------------------------------------------------------------
# install.packages('factoextra', "FactoMineR")
# install.packages('tibble')
# install.packages('tidyverse')
# install.packages('tidyselect')
# install.packages('corrplot')

library(tibble)
library(tidyverse)
library("FactoMineR")
library("factoextra")
library(corrplot)

# Principe de l'ACP
# ACP
# ------------------------------------------------------------------------------------------------------------
X = seq(1:100)
Y = 2*X + rnorm(100, mean=0, sd=20)
plot(Y~X)

df=data.frame(X,Y)
df

library(ade4)
pca = dudi.pca(df, scannf = FALSE, nf = 2)
inertia.dudi(pca)
scatter(pca)
pca$li

plot(pca$li[,1] ~ seq(1:100)) 
plot(pca$li[,2] ~ seq(1:100)) 


# ACP
# ------------------------------------------------------------------------------------------------------------



dim(decathlon)
str(decathlon)
rownames(decathlon)


data(decathlon2)
str(decathlon2)
head(decathlon2)

data("decathlon")
decathlon
dim(decathlon)
head(decathlon, 4)
str(decathlon)
rownames(decathlon)

decathlon[,1:10]
plot(decathlon[,1:10])
cor(decathlon[,1:10])
heatmap(cor(decathlon[,1:10]))

library(corrplot)
corrplot(cor(decathlon2[,1:10]))

str(decathlon2)



decathlon2.active <- decathlon2[1:23, 1:10]

head(decathlon2.active, 4)
dim(decathlon2.active)


# PCA

res.pca <- PCA(decathlon2.active, 
               scale.unit = TRUE, 
               graph = FALSE)
print(res.pca)

res.pca$eig

eig.val <- get_eigenvalue(res.pca)
eig.val


fviz_eig(res.pca, addlabels = TRUE, 
         ylim = c(0, 50))


# variables
var <- get_pca_var(res.pca)
var

# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)


# variables

head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")


head(var$cos2, 4)
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)


# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var",
          axes = 1:2)



# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", 
                               "#E7B800", 
                               "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)



# individus

fviz_pca_ind(res.pca)


fviz_pca_ind(res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

fviz_pca_ind (res.pca, pointsize = "cos2",
              pointshape = 21, fill = "#E7B800",
              repel = TRUE # Évite le chevauchement de texte
)

fviz_cos2(res.pca, choice = "ind")




fviz_pca_ind(res.pca,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = decat3$Competition[1:36], # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)



fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969" ,
                axes = c(1,3)# Couleur des individues
)


# ind et var supplementaire	

res.pca <- PCA(decathlon, ind.sup = 37:41, 
               quanti.sup = 11:12,
               quali.sup = 13, graph=FALSE)

res.pca$quanti.sup
fviz_pca_var(res.pca, axes = c(1,3))


p <- fviz_pca_ind(res.pca, col.ind.sup = "blue", repel = TRUE)
p <- fviz_add(p, res.pca$quali.sup$coord, color = "red")
p

fviz_pca_ind(res.pca, habillage = 13,
             addEllipses =TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE) 
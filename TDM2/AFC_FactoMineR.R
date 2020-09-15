library("FactoMineR")
library("factoextra")

data(housetasks)
head(housetasks)


# 
# Les données correspondent à un tableau de contingence contenant 13 taches ménagères et leur répartition dans le couple:
#   
#   les lignes sont les différentes tâches
# les valeurs sont les fréquences des tâches effectuées:
#   par la femme seulement (colonne "wife")
# alternativement (colonne "alternatively")
# par le mari seulement (colonne "husband")
# ou conjointement (colonne "jointly")



library("gplots")
# 1. convertir les données en tant que table
dt <- as.table(as.matrix (housetasks))
# 2. Graphique
balloonplot(t (dt), main = "housetasks", 
            xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)



chisq <- chisq.test (housetasks)
chisq


# CA
res.ca <- CA(housetasks, graph = FALSE)
print(res.ca)

library ("factoextra")
eig.val <- get_eigenvalue (res.ca)
eig.val


fviz_eig (res.ca, addlabels = TRUE, ylim = c(0, 50))



fviz_ca_biplot (res.ca, repel = TRUE)

# 
# Dans le graphique ci-dessus, les lignes sont représentées par des points bleus et des colonnes par des triangles rouges.
# 
# La distance entre les points lignes ou entre les points colonnes donne une mesure de leur similitude (ou dissemblance). Les points lignes avec un profil similaire sont proches sur le graphique. Il en va de même pour les points colonnes.
# 
# Ce graphique montre que:
#   
#   Les lignes Dinner, Breakfeast et Laundry sont associées le le plus à la colonne Wife
# Les lignes Driving et Repairs sont associées le plus à la colonne Husband.
# ..



row <- get_ca_row(res.ca)
row


# Coordonnées
head(row$coord)
# Cos2: qualité de représentation
head(row$cos2)
# Contributions 
head(row$contrib)


head(row$coord)
fviz_ca_row(res.ca, repel = TRUE)
fviz_ca_row (res.ca, col.row = "steelblue", shape.row = 15)

head(row$cos2, 4)
# Colorer en fonction du cos2:
fviz_ca_row (res.ca, col.row = "cos2",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_ca_row (res.ca, col.row = "contrib",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

library("corrplot")
corrplot(row$cos2, is.corr = FALSE)

# Cos2 des lignes sur Dim.1 et Dim.2
fviz_cos2(res.ca, choice = "row", axes = 1:2)


head(row$contrib)
library("corrplot")
corrplot(row$contrib, is.corr=FALSE) 

# Contributions des lignes à la dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions des lignes à la dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)


fviz_ca_row (res.ca, col.row = "contrib",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



# Le graphique donne une idée de la contribution des lignes aux différents pôles des dimensions.
# 
# Il est évident que les catégories Repair et Driving ont une contribution importante au pôle positif de la première dimension, tandis que les catégories Laundry et Main_meal ont une contribution majeure au pôle négatif de la première dimension; etc, ..
# 
# En d'autres termes, la dimension 1 est principalement définie par l'opposition de Repair et Driving (pôle positif) avec Laundry et Main_meal (pôle négatif).







col <- get_ca_col(res.ca)
col


# Coordonnées
head(col$coord)
# Qualité de représentation
head(col$cos2)
# Contributions
head(col$contrib)


fviz_ca_col (res.ca, col.col = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_cos2 (res.ca, choice = "col", axes = 1:2)


fviz_ca_biplot(res.ca, repel = TRUE)
# Avec un biplot symétrique, la distance entre les lignes et les colonnes ne peut pas être interprétée. Seules des conclusions générales peuvent être tirées



fviz_ca_biplot (res.ca,
                map = "rowprincipal", arrow = c(TRUE, TRUE),
                repel = TRUE)
# 
# Nous avons utilisé, l'argument arrow, qui est un vecteur logique, de longueur 2, spécifiant si le graphique doit contenir des points (FALSE, par défaut) ou des flèches (TRUE). La première valeur définit les lignes et la seconde valeur définit les colonnes.
# 
# Si l'angle entre deux flèches est aigu, alors il y a une forte association entre les lignes et les colonnes correspondantes.
# 
# Pour interpréter la distance entre les lignes et les colonnes, vous devriez projeter perpendiculairement des points lignes sur la flèche de la colonne.
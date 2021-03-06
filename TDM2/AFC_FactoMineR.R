library("FactoMineR")
library("factoextra")

data(housetasks)
head(housetasks)


# 
# Les donn�es correspondent � un tableau de contingence contenant 13 taches m�nag�res et leur r�partition dans le couple:
#   
#   les lignes sont les diff�rentes t�ches
# les valeurs sont les fr�quences des t�ches effectu�es:
#   par la femme seulement (colonne "wife")
# alternativement (colonne "alternatively")
# par le mari seulement (colonne "husband")
# ou conjointement (colonne "jointly")



library("gplots")
# 1. convertir les donn�es en tant que table
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
# Dans le graphique ci-dessus, les lignes sont repr�sent�es par des points bleus et des colonnes par des triangles rouges.
# 
# La distance entre les points lignes ou entre les points colonnes donne une mesure de leur similitude (ou dissemblance). Les points lignes avec un profil similaire sont proches sur le graphique. Il en va de m�me pour les points colonnes.
# 
# Ce graphique montre que:
#   
#   Les lignes Dinner, Breakfeast et Laundry sont associ�es le le plus � la colonne Wife
# Les lignes Driving et Repairs sont associ�es le plus � la colonne Husband.
# ..



row <- get_ca_row(res.ca)
row


# Coordonn�es
head(row$coord)
# Cos2: qualit� de repr�sentation
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

# Contributions des lignes � la dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions des lignes � la dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)


fviz_ca_row (res.ca, col.row = "contrib",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



# Le graphique donne une id�e de la contribution des lignes aux diff�rents p�les des dimensions.
# 
# Il est �vident que les cat�gories Repair et Driving ont une contribution importante au p�le positif de la premi�re dimension, tandis que les cat�gories Laundry et Main_meal ont une contribution majeure au p�le n�gatif de la premi�re dimension; etc, ..
# 
# En d'autres termes, la dimension 1 est principalement d�finie par l'opposition de Repair et Driving (p�le positif) avec Laundry et Main_meal (p�le n�gatif).







col <- get_ca_col(res.ca)
col


# Coordonn�es
head(col$coord)
# Qualit� de repr�sentation
head(col$cos2)
# Contributions
head(col$contrib)


fviz_ca_col (res.ca, col.col = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_cos2 (res.ca, choice = "col", axes = 1:2)


fviz_ca_biplot(res.ca, repel = TRUE)
# Avec un biplot sym�trique, la distance entre les lignes et les colonnes ne peut pas �tre interpr�t�e. Seules des conclusions g�n�rales peuvent �tre tir�es



fviz_ca_biplot (res.ca,
                map = "rowprincipal", arrow = c(TRUE, TRUE),
                repel = TRUE)
# 
# Nous avons utilis�, l'argument arrow, qui est un vecteur logique, de longueur 2, sp�cifiant si le graphique doit contenir des points (FALSE, par d�faut) ou des fl�ches (TRUE). La premi�re valeur d�finit les lignes et la seconde valeur d�finit les colonnes.
# 
# Si l'angle entre deux fl�ches est aigu, alors il y a une forte association entre les lignes et les colonnes correspondantes.
# 
# Pour interpr�ter la distance entre les lignes et les colonnes, vous devriez projeter perpendiculairement des points lignes sur la fl�che de la colonne.
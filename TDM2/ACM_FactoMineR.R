
###


library("FactoMineR")
library("factoextra")
library(tidyverse)

data (hobbies)
summary (hobbies)
head(hobbies)



# Le jeu de donnée est pris directement dans le package FactoMineR : 
# il s'agit d'un dataframe rassemblant des données concernant les hobbies des personnes
# interrogées. Pour l'analyse, on ne retient que les variables concernant ces pratiques,
# en mettant de côté les variables décrivant les individus eux-mêmes, soit les variables 
# 19 à 23 du dataframe.

library(ade4)
hob = select (hobbies, -(19:23)) 
acm.disjonctif(hob)
acm.burt(hob, hob)

# Valeurs propres et inertie moyenne
acm <- select (hobbies, -(19:23)) %>% MCA (graph = F)
names(acm)

head (acm$eig)

eig <- as.data.frame (acm$eig)
mean (eig$`percentage of variance`)

# Ici, le critère de l'inertie moyenne conduirait à retenir 5 voire 6 axes 
# factoriels qui portent 40 à 45% de l'hétérogénéité


acm$eig[,1] %>% diff() %>% diff()

# Le critère du coude est lui nettement plus parcimonieux et conduit à retenir 3 axes 
# : la dérivée seconde change de signe entre les axes 3 et 4. Mais dans ce cas, 
# seule 30% de l'inertie est conservée.


mm <- mean(eig$`percentage of variance`)
ggplot(eig, aes(x = 1:nrow(eig), weight = `percentage of variance`)) +
  geom_bar (fill = "lightblue") + 
  coord_flip() + ggtitle ("Eboulis des valeurs propres") + 
  theme (axis.title = element_blank()) +
  geom_hline (yintercept = mm)
# Dans une ACM, on n'a pas forcément de "décrochage" évident dans l'éboulis des 
# valeurs propres : il est plus difficile de concentrer l'inertie de variables 
# qualitatives que quantitatives. On doit donc généralement retenir plus d'axes
# ou renoncer à une part significative de l'inertie.
# 
# En raison de cette dilution de l'information dans l'ACM, quand on dispose 
# de variables qualitatives et quantitatives, il est souvent préférable de 
# faire une ACP en introduisant les variable qualitatives en variables illustratives
# (si elles ne sont pas cruciales, évidemment).
# 
# On peut aussi retenir un nombre d'axes assez élevé, puis dans un second temps
# ne conserver que ceux qui sont bien interprétables.



plot.MCA (acm, axes = 1:2, cex = 0.7, invisible = "ind")
plot.MCA (acm, axes = 1:2, cex = 0.7, invisible = "ind", selectMod = "contrib 15")

plot.MCA (acm, axes = 3:4, cex = 0.7, invisible = "ind", selectMod = "cos2 15")


# Les résultats sur le premier plan factoriel sont assez parlants :
#   le premier axe sépare les modalités "pratique" versus "non pratique" de l'acitivité.
# Le second axe semble opposer les activités manuelles ou physiques (coordonnées positives
#                                                                    sur l'axe 2) aux 
# activités plus culturelles (cinéma, musique.).

# Sur le deuxième plan factoriel, l'axe 3 permet de distinguer les individus 
# qui pêchent et écoutent de la musique des individus qui font de la couture 
# et de la cuisine. Le 4^ème axe factoriel met en évidence les individus qui
# ne regardent pas la télévision et qui s'impliquent dans le volontariat.

plot.MCA(acm,axes=1:2,cex=.7,selectMod = "cos2 10",select = "contrib 10", invisible = "ind")





acm <- MCA(hobbies,quali.sup = 19:22,quanti.sup = 23,ncp=4,graph = F)
plot.MCA(acm,axes=1:2,cex=.7,selectMod = "cos2 10",select = "contrib 10", invisible = "ind")
# On voit notamment que ce sont les moins qualifiés et les plus âgés qui ont 
# moins de hobby,
# alors que les managers sont plutôt dans le cadrant des loisirs "culturels".
# 

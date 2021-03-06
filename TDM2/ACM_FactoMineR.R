
###


library("FactoMineR")
library("factoextra")
library(tidyverse)

data (hobbies)
summary (hobbies)
head(hobbies)



# Le jeu de donn�e est pris directement dans le package FactoMineR : 
# il s'agit d'un dataframe rassemblant des donn�es concernant les hobbies des personnes
# interrog�es. Pour l'analyse, on ne retient que les variables concernant ces pratiques,
# en mettant de c�t� les variables d�crivant les individus eux-m�mes, soit les variables 
# 19 � 23 du dataframe.

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

# Ici, le crit�re de l'inertie moyenne conduirait � retenir 5 voire 6 axes 
# factoriels qui portent 40 � 45% de l'h�t�rog�n�it�


acm$eig[,1] %>% diff() %>% diff()

# Le crit�re du coude est lui nettement plus parcimonieux et conduit � retenir 3 axes 
# : la d�riv�e seconde change de signe entre les axes 3 et 4. Mais dans ce cas, 
# seule 30% de l'inertie est conserv�e.


mm <- mean(eig$`percentage of variance`)
ggplot(eig, aes(x = 1:nrow(eig), weight = `percentage of variance`)) +
  geom_bar (fill = "lightblue") + 
  coord_flip() + ggtitle ("Eboulis des valeurs propres") + 
  theme (axis.title = element_blank()) +
  geom_hline (yintercept = mm)
# Dans une ACM, on n'a pas forc�ment de "d�crochage" �vident dans l'�boulis des 
# valeurs propres : il est plus difficile de concentrer l'inertie de variables 
# qualitatives que quantitatives. On doit donc g�n�ralement retenir plus d'axes
# ou renoncer � une part significative de l'inertie.
# 
# En raison de cette dilution de l'information dans l'ACM, quand on dispose 
# de variables qualitatives et quantitatives, il est souvent pr�f�rable de 
# faire une ACP en introduisant les variable qualitatives en variables illustratives
# (si elles ne sont pas cruciales, �videmment).
# 
# On peut aussi retenir un nombre d'axes assez �lev�, puis dans un second temps
# ne conserver que ceux qui sont bien interpr�tables.



plot.MCA (acm, axes = 1:2, cex = 0.7, invisible = "ind")
plot.MCA (acm, axes = 1:2, cex = 0.7, invisible = "ind", selectMod = "contrib 15")

plot.MCA (acm, axes = 3:4, cex = 0.7, invisible = "ind", selectMod = "cos2 15")


# Les r�sultats sur le premier plan factoriel sont assez parlants :
#   le premier axe s�pare les modalit�s "pratique" versus "non pratique" de l'acitivit�.
# Le second axe semble opposer les activit�s manuelles ou physiques (coordonn�es positives
#                                                                    sur l'axe 2) aux 
# activit�s plus culturelles (cin�ma, musique.).

# Sur le deuxi�me plan factoriel, l'axe 3 permet de distinguer les individus 
# qui p�chent et �coutent de la musique des individus qui font de la couture 
# et de la cuisine. Le 4^�me axe factoriel met en �vidence les individus qui
# ne regardent pas la t�l�vision et qui s'impliquent dans le volontariat.

plot.MCA(acm,axes=1:2,cex=.7,selectMod = "cos2 10",select = "contrib 10", invisible = "ind")





acm <- MCA(hobbies,quali.sup = 19:22,quanti.sup = 23,ncp=4,graph = F)
plot.MCA(acm,axes=1:2,cex=.7,selectMod = "cos2 10",select = "contrib 10", invisible = "ind")
# On voit notamment que ce sont les moins qualifi�s et les plus �g�s qui ont 
# moins de hobby,
# alors que les managers sont plut�t dans le cadrant des loisirs "culturels".
# 

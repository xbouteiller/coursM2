# livre de legendre : numerical ecology with R
# une référence
# http://jinliangliu.weebly.com/uploads/2/5/7/8/25781074/numerical_ecology_with_r.pdf


#  -------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------

# exemple RDA
# https://www.davidzeleny.net/anadat-r/doku.php/en:rda_cca_examples

# RDA, tb-RDA, CCA & db-RDA (constrained ordination)

vltava.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)
vltava.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-env.txt')

spe <- vltava.spe  # rename variables to make them shorter
# env <- vltava.env
env <- vltava.env[, c('pH', 'SOILDPT')]

str(vltava.spe)
str(vltava.env)

library (vegan)
spe.log <- log1p (spe)  # species data are in percentage scale which is strongly rightskewed, better to transform them
spe.hell <- decostand (spe.log, 'hell')  # we are planning to do tb-RDA, this is Hellinger pre-transformation
# tbRDA <- rda (spe.hell ~ ., data = env)  
tbRDA <- rda (spe.hell ~ pH + SOILDPT, data = env)  # calculate tb-RDA with two explanatory variables
tbRDA


# The two variables explain 8.9% of the variance (the row Constrained and column Proportion in the 
# table above, can be calculated also as the sum of eigenvalues for the constrained axes divided
# by total variance (inertia): (0.04023+0.02227) /0.70476=0.08869. The first constrained axis (RDA1) 
# explains 0.04023/0.70476=5.7% of the variance, while the second (RDA2) explains 0.02227/0.70476=3.2%.
# Note that the first unconstrained axis (PC1) represents 0.07321/0.70476=10.4% of the total variance, 
# which is more than the variance explained by both explanatory variables together;
# the first two unconstrained explain (0.07321+0.04857)/0.70476=17.3%. This means that
# the dataset may be structured by some strong environmental variable(s) different 
# from pH and soil depth (we will check this below). 

constrained_eig <- tbRDA$CCA$eig/tbRDA$tot.chi*100
unconstrained_eig <- tbRDA$CA$eig/tbRDA$tot.chi*100
expl_var <- c(constrained_eig, unconstrained_eig)
barplot (expl_var[1:20], col = c(rep ('red', length (constrained_eig)), rep ('black', length (unconstrained_eig))),
         las = 2, ylab = '% variation')


ordiplot (tbRDA)


# By the way, what may be those environmental variables associated with unconstrained axes? 
# The vltava.env dataset contains a number of other measured variables which we may fit as 
# supplementary to the first and second unconstrained axis to see which of them is most related
# to which of them. But here we will do an alternative thing: 
# we will use mean Ellenberg indicator values (mEIV) calculated for each plot
# based on the species composition and tabulated Ellenberg species indicator values
# (ecological optima of species along several main environmental gradients). This approach
# will illustrate the situation as if in the field we measured only soil pH and depth
# (which are rather easy to measure), and we use these indirect estimates 
# to get an idea about which other factors may be important. 

# https://davidzeleny.net/wiki/doku.php/eiv:start
?envfit

ordiplot(tbRDA, choices = c(3,4), type = 'n')
points(tbRDA, choices = c(3,4), display = 'sites',
       pch = as.character(vltava.env$GROUP), col = vltava.env$GROUP)
ef <- envfit (tbRDA, vltava.env[,23:28], choices = c(3,4), permutations = 0)
plot (ef)

ef

# The highest R2 of regression with the first two axes have light and moisture, with light associated
# mostly with the first unconstrained axis and the moisture mostly with the second.
# It seems that these two ecological factors, not related to soil pH and soil depth, 
# are important for the studied vegetation, but were not measured; light passing through
# the canopy of the forest has a strong effect on the species composition of the herb
# understory (herbs makes most of the species in this analysis since the temperate forest
# is rather poor for woody species), and moisture also (flooded alluvial forests
# at the bottom parts of the valley, veg. type 2, have very different species composition from dry
# open forests on the upper parts of the valley slopes).

# Note one more thing: when applying the function envfit on mean Ellenberg indicator values,
# I did not test for the significance (I set the argument permutation = 0). This has a meaning:
#   both mean Ellenberg indicator values and sample scores on ordination axes are calculated 
# from the same matrix of species composition, and to directly test their relationship would 
# be wrong (they are not independent, and we get high probability to get significant result even
#           if the species Ellenberg indicator values are randomly generated). Check the section
# Analysis of species attributes for detail explanation on how to solve this


#  -------------------------------------------------------------------------------------------------
#  -------------------------------------------------------------------------------------------------
# RDA data doubs inspired from legendre

# http://jinliangliu.weebly.com/uploads/2/5/7/8/25781074/numerical_ecology_with_r.pdf
library(ade4)
library(adegraphics)
data(doubs)
str(doubs)

doubs$env
doubs$fish
names(doubs$fish) = doubs$species$French


#  EXPLO -------------------------------------------------------------------------------------------------
?table.value
dev.off()
table.value(doubs$fish, csize=0.3)

s.value(doubs$xy, scale(apply(ifelse(doubs$fish>0,1,0),M=1, sum)), method = c( "greylevel"), zmax=5)
s.label(doubs$xy,label= row.names(doubs$xy),
        add.plot = TRUE, clabel= 0.7, boxes = FALSE)

s.value(doubs$xy, scale(apply(ifelse(doubs$fish>0,1,0),M=1, sum)), zmax=2.5)
s.label(doubs$xy,label= row.names(doubs$xy),
        add.plot = TRUE, clabel= 0.7, boxes = FALSE)



presabs = ifelse(doubs$fish>0,1,0)
par(mfrow=n2mfrow(ncol(doubs$fish)))
for(i in 1:ncol(doubs$fish)){
  s.value(doubs$xy,presabs[,i], sub = colnames(presabs)[i], zmax=4)
  s.label(doubs$xy,label= row.names(doubs$xy),
          add.plot = TRUE, clabel= 0.7, boxes = FALSE)
}


#  ENV DATA -------------------------------------------------------------------------------------------------

pca=dudi.pca(doubs$env, scannf = FALSE, nf=2)
inertia.dudi(pca)
scatter(pca)
inertia.dudi(pca, col = TRUE)


#  FISH DATA -------------------------------------------------------------------------------------------------


spe.hel <- decostand (doubs$fish, 'hell')

library(FactoMineR)

res.ca <- CA(doubs$fish, graph = FALSE)
#or
res.ca <- CA(presabs, graph = FALSE)
print(res.ca)

#or PCA on transformed data see legendre P128-130
# res.ca <- PCA(spe.hel, graph = FALSE)


library ("factoextra")
eig.val <- get_eigenvalue (res.ca)
eig.val


fviz_eig(res.ca, addlabels = TRUE, ylim = c(0, 50))


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

# Comme mentionné ci-dessus, le graphique standard de l'analyse factorielle des correspondances est un 
# biplot symétrique dans lequel les lignes (points bleus) et les colonnes (triangles rouges) sont
# représentées dans le même espace à l'aide des coordonnées principales. Ces coordonnées représentent
# les profils des lignes et des colonnes. Dans ce cas, seule la distance
# entre les points lignes ou la distance entre les points colonnes peut être vraiment interprétée

# Avec un biplot symétrique, la distance entre les lignes et les colonnes ne peut pas être interprétée.
# Seules des conclusions générales peuvent être tirées.

fviz_ca_biplot (res.ca, repel = TRUE)


# Notez que, pour interpréter la distance entre les points colonnes et les points lignes, 
# le moyen le plus simple est de créer un biplot asymétrique. Cela signifie que les profils des
# colonnes doivent être représentés dans l'espace des lignes ou vice versa. 

fviz_ca_biplot (res.ca,
                map = "rowprincipal", arrow = c(TRUE, TRUE),
                repel = TRUE)

# Si l'angle entre deux flèches est aigu, alors il y a une forte association entre les 
# lignes et les colonnes correspondantes.
# 
# Pour interpréter la distance entre les lignes et les colonnes, vous devriez 
# projeter perpendiculairement des points lignes sur la flèche de la colonne.

#  RDA -------------------------------------------------------------------------------------------------

library(vegan)

fish = doubs$fish[-8,]
env = doubs$env[-8,]

# Remove the 'dfs' variable from the env data frame
env2 <- env[, -1]
# Recode the slope variable (slo) into a factor (qualitative) 
# variable to show how these are handled in the ordinations
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, 
               levels = c(".low", ".moderate", ".steep", ".very_steep"))
table(slo2)
# Create an env3 data frame with slope as a qualitative variable
env3 <- env2
env3$slo <- slo2



# spe.hel <- decostand (spe.log, 'hell')  # we are planning to do tb-RDA, this is Hellinger pre-transformation

spe.hel <- decostand (fish, 'hell')



## RDA of the Hellinger-transformed fish species data, constrained
## by all the environmental variables contained in env3
(spe.rda <- rda(spe.hel ~ ., env3)) # Observe the shortcut formula
summary(spe.rda)	# Scaling 2 (default)


# 
# Conceptually, RDA is a multivariate (meaning multiresponse) multiple linear
# regression followed by a PCA of the table of fitted values. It works as follows, on
# a matrix Y of centred response data and a matrix X of centred (or, more generally,
#                                                                standardized) explanatory variables



constrained_eig <- spe.rda$CCA$eig/spe.rda$tot.chi*100
unconstrained_eig <- spe.rda$CA$eig/spe.rda$tot.chi*100
expl_var <- c(constrained_eig, unconstrained_eig)
barplot (expl_var[1:20], col = c(rep ('red', length (constrained_eig)), rep ('black', length (unconstrained_eig))),
         las = 2, ylab = '% variation')


# Canonical coefficients from the rda object
coef(spe.rda)

?RsquareAdj
# Unadjusted R^2 retrieved from the rda object
(R2 <- RsquareAdj(spe.rda)$r.squared)
# Adjusted R^2 retrieved from the rda object
(R2adj <- RsquareAdj(spe.rda)$adj.r.squared)


## Triplots of the rda results (wa scores)
## Site scores as weighted averages (vegan's default)
# Scaling 1 :  distance triplot
dev.new(title = "RDA scaling 1 + wa", noRStudioGD = TRUE)
plot(spe.rda, 
     scaling = 1, 
     main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores"
)
arrows(0, 0, 
       spe.sc1[, 1] * 0.92, 
       spe.sc1[, 2] * 0.92, 
       length = 0, 
       lty = 1, 
       col = "red"
)

# Scaling 2 (default) :  correlation triplot
dev.new(title = "RDA scaling 2 + wa", noRStudioGD = TRUE)
plot(spe.rda, 
     main = "Triplot RDA spe.hel ~ env3 - scaling 2 - wa scores")
arrows(0, 0, 
       spe.sc2[, 1] * 0.92, 
       spe.sc2[, 2] * 0.92, 
       length = 0, 
       lty = 1, 
       col = "red"
)




# For the species and sites, the interpretation of the two scalings is the same as in
# PCA. However, the presence of vectors and centroids of explanatory variables calls
# for additional interpretation rules. Here are the essential ones (see Legendre and
#                                                                   Legendre 1998, pp. 586-587):

#   . Scaling 1 - distance biplot: (1) Projecting an object at right angle on a response
# variable or a quantitative explanatory variable approximates the position of
# the object along that variable. (2) The angles between response and explanatory
# variables in the biplot reflect their correlations (but not the angles among
#                                                     response variables). (3) The relationship between the centroid of a qualitative
# explanatory variable and a response variable (species) is found by projecting the
# centroid at right angle on the species variable, as for individual objects, since we
# are projecting the centroid of a group of objects. (4) Distances among centroids,
# and between centroids and individual objects, approximate their Euclidean
# distances.

# . Scaling 2 - correlation biplot: (1) Projecting an object at right angle on a
# response or a quantitative explanatory variable approximates the value of the
# object along that variable. (2) The angles in the biplot between response and
# explanatory variables, and between response variables themselves or explanatory
# variables themselves, reflect their correlations. (3) The relationship
# between the centroid of a qualitative explanatory variable and a response variable
# (species) is found by projecting the centroid at right angle on the species






# 
# These triplots show that oxygen (oxy), altitude (alt), nitrates (nit) and
# discharge
# (deb), as well as slope (mainly the level penvery_steep) play an
# important role in the dispersion of the sites along the first axis. Both triplots oppose
# the upper and lower parts of the river along the first axis. The scaling 2 triplot
# shows three groups of fish species correlated with different sets of explanatory
# variables: the brown trout (TRU), Eurasian minnow (VAI) and stone loach (LOC)
# are found in the first half of the sites, and are correlated with high oxygen content
# and slope as well as higher altitude. The bleak (ABL), roach (GAR) and European
# chub (CHE), on the opposite, are related to sites 23, 24 and 25 characterized by high
# phosphates (pho), ammonium (amm) and biological oxygen demand (dbo) levels.
# Most other species are bunched together away from these extremes. They show
# mostly shorter projections, indicating that they are either present over most portions
# of the river or related to intermediate ecological conditions




# Global test of the RDA result
anova(spe.rda, permutations = how(nperm = 999))
# Tests of all canonical axes
anova(spe.rda, by = "axis", permutations = how(nperm = 999))
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


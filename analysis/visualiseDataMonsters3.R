library(ape)
# Set working directory
setwd("C:/Users/tknow/Desktop/project/analysis")

# Load monsters data
d = read.csv("../data/clean/monsters.csv",stringsAsFactors = F)

# Make sure class and caste are categorical variables
d$class.stratified = factor(d$class.stratified, labels = c("Not stratified", "Class stratified"))
d$caste.stratified = factor(d$caste.stratified, labels = c("Not stratified", "Caste stratified")) 

# Urbanization as ordered category
d$urban = factor(d$urban, ordered = TRUE, 
                 labels = c("< 100 persons", 
                            "100-199 persons", 
                            "200-399 persons", 
                            "400-999 persons", 
                            "1000+ persons"))

# Agriculture as ordered category
d$ag = factor(d$ag, ordered = TRUE, 
                 labels = c("None", 
                            "10% food supply", 
                            "10 %; secondary", 
                            "Primary; not intensive", 
                            "Primary; intensive"))


# Population Density as ordered category
d$popdens = factor(d$popdens, ordered = TRUE, 
                 labels = c("< 1 person / sq. mile", 
                            "1-5 persons / sq. mile", 
                            "5.1-25 persons/ sq. mile", 
                            "26-100 persons  / sq. mile", 
                            "100 persons / sq. mile"))


# Fixity of Residence as ordered category
d$fixity = factor(d$fixity, ordered = TRUE, 
                 labels = c("Nomadic", 
                            "Seminomadic", 
                            "Semisedentary", 
                            "Sedentary; impermanent", 
                            "Sedentary"))


# Land Transport as ordered category
d$land = factor(d$land, ordered = TRUE, 
                 labels = c("Human only", 
                            "Pack Animals", 
                            "Draft Animals", 
                            "Animal-drawn vehicles", 
                            "Automotive vehicles"))


# Money as ordered category
d$money = factor(d$money, ordered = TRUE, 
                 labels = c("None", 
                            "Domestically usable particles", 
                            "Alien currency", 
                            "Elementary forms", 
                            "True money"))



# Political Integration as ordered category
d$politic = factor(d$politic, ordered = TRUE, 
                 labels = c("None", 
                            "Autonomous local communities", 
                            "1 level above community", 
                            "2 levels above community", 
                            "3 levels above community"))

# Social Stratification as ordered category
d$strata = factor(d$strata, ordered = TRUE, 
                 labels = c("Egalitarian", 
                            "Wealth Differences or hereditary slavery", 
                            "2 social classes, no castes/slavery", 
                            "2 social classes, castes/slavery", 
                            "3 social classes or castes, w/ or w/out slavery"))

# Technological Specialization as ordered category
d$tech = factor(d$tech, ordered = TRUE, 
                 labels = c("None", 
                            "Pottery only", 
                            "Loom weaving only", 
                            "Metalwork only", 
                            "Smiths, weavers, potters"))

# Writing and Records as ordered category
d$writing = factor(d$writing, ordered = TRUE, 
                 labels = c("None", 
                            "Mnemonic devices", 
                            "Nonwritten records", 
                            "True writing; no records", 
                            "True writing, records"))

# Market Exchange as ordered category
d$market = factor(d$market, ordered = TRUE, labels = c("Local", 
                                                       "Outside Local", 
                                                       "Regional",
                                                       "Supra-Regional"))

# Collapse land transport categories 4 and 5
d$land = forcats::fct_collapse(d$land, 
                               "Human only"   = c("Human only"),
                               "Pack Animals"  = c("Pack Animals"),
                               "Draft Animals" = c("Draft Animals"),
                               "Wheeled Vehicles"   = c("Animal-drawn vehicles", "Automotive vehicles"))


# Collapse technological specialization
d$tech = forcats::fct_collapse(d$tech,
                               "None" = c("None"),
                               "Pottery or Loom" = c("Pottery only", "Loom weaving only"),
                               "Metallurgy" = c("Metalwork only"),
                               "Multiple specialists" = c("Smiths, weavers, potters"))



# Collapse True writing in Writing
d$writing = forcats::fct_collapse(d$writing, 
                                  "None" = c("None"), 
                                  "Mnemonic devices" = c("Mnemonic devices"), 
                                  "Nonwritten records" = c("Nonwritten records"), 
                                  "True writing" = c("True writing; no records", "True writing, records"))
                                  
                                  
View(d)

# Monster ~ class stratified
fisher.test(table(d$monster_present,d$class.stratified))

# Monster ~ urbanization
fisher.test(table(d$monster_present,d$urban))

# Monster ~ agriculture
fisher.test(table(d$monster_present,d$ag))

# Monster ~ population density
fisher.test(table(d$monster_present,d$popdens))

# Monster ~ fixity of residence
fisher.test(table(d$monster_present,d$fixity))

# Monster ~ land transport
fisher.test(table(d$monster_present,d$land))

# Monster ~ money
fisher.test(table(d$monster_present,d$money))

# Monster ~ political integration
fisher.test(table(d$monster_present,d$politic))

# Monster ~ social stratification
fisher.test(table(d$monster_present,d$strata))

# Monster ~ technological specialization
fisher.test(table(d$monster_present,d$tech))

# Monster ~ writing and records
fisher.test(table(d$monster_present,d$writing))


# Monster ~ market exchange
fisher.test(table(d$monster_present,d$market))

# Extra tests for market exchange:
# Excluding societies with category "Outside Local"
dCertainMarketOnly = d[d$market!="Outside Local",]
dCertainMarketOnly$market = factor(dCertainMarketOnly$market)
fisher.test(table(dCertainMarketOnly$monster_present,dCertainMarketOnly$market))



# Class results table
table1 = table(monster = d$monster_present, class = d$class.stratified)
row.names(table1) = c("no hybrids", "hybrids")
write.csv(table1, "../results/tables/table1.csv")

# Urbanization results table
table2 = table(monster = d$monster_present, urbanization = d$urban)
row.names(table2) = c("no hybrids", "hybrids")
colnames(table2) = c("<100 persons", "100-199 persons", "200-399 persons", "400-999 persons", "1000+ persons")
write.csv(table2, "../results/tables/table2.csv")

# Agriculture results table
table3 = table(monster = d$monster_present, agriculture = d$ag)
row.names(table3) = c("no hybrids", "hybrids")
write.csv(table3, "../results/tables/table3.csv")

# Population Density results table
table4 = table(monster = d$monster_present, density = d$popdens)
row.names(table4) = c("no hybrids", "hybrids")
write.csv(table4, "../results/tables/table4.csv")

# Fixity of Residence results table
table5 = table(monster = d$monster_present, fixity = d$fixity)
row.names(table5) = c("no hybrids", "hybrids")
write.csv(table5, "../results/tables/table5.csv")

# Land Transport results table
table6 = table(monster = d$monster_present, transport = d$land)
row.names(table6) = c("no hybrids", "hybrids")
write.csv(table6, "../results/tables/table6.csv")

# Money results table
table7 = table(monster = d$monster_present, money = d$money)
row.names(table7) = c("no hybrids", "hybrids")
write.csv(table7, "../results/tables/table7.csv")

# Political Integration results table
table8 = table(monster = d$monster_present, integration = d$politic)
row.names(table8) = c("no hybrids", "hybrids")
write.csv(table8, "../results/tables/table8.csv")

# Social Stratification results table
table9 = table(monster = d$monster_present, stratification = d$strata)
row.names(table9) = c("no hybrids", "hybrids")
write.csv(table9, "../results/tables/table9.csv")

# Technological Specialization results table
table10 = table(monster = d$monster_present, technology = d$tech)
row.names(table10) = c("no hybrids", "hybrids")
write.csv(table10, "../results/tables/table10.csv")

# Writing and Records results table
table11 = table(monster = d$monster_present, records = d$writing)
row.names(table11) = c("no hybrids", "hybrids")
write.csv(table11, "../results/tables/table11.csv")

# Market results table
table12 = table(monster = d$monster_present, marketExchange = d$market)
row.names(table12) = c("no hybrids", "hybrids")
write.csv(table12, "../results/tables/table12.csv")


# Which variables to look at in combination? # Excludes market & class
# Use a decision tree:
library(party)
decisionTree = ctree(factor(monster_present) ~ urban + ag + popdens + fixity + land + money + politic + strata + tech + writing, data = d)
plot(decisionTree)

# Code for random forest # Excludes market & class
randomForest = cforest(factor(monster_present) ~ urban + ag + popdens + fixity + land + money + politic + strata + tech + writing, data = d)
importance = varimp(randomForest)
library(lattice)
dotplot(sort(importance),xlab="Variable Importance")

# Random forest including Market (and therefore smaller data set)
randomForest = cforest(factor(monster_present) ~ market + urban + ag + popdens + fixity + land + money + politic + strata + tech + writing, data = d)
importance = varimp(randomForest)
library(lattice)
dotplot(sort(importance),xlab="Variable Importance")


# Code for changing threshold for decision tree (change 0.95 lower to have a less strict threshold):
decisionTree = ctree(factor(monster_present) ~ urban + ag + popdens + fixity + land + money + politic  + strata + tech + writing, data = d, 
                     controls = ctree_control(mincriterion = 0.95))


# Code to plot proportions of cultures with and without hybrids:
  
par(mar=c(3,10,2,1))
propHybridsByWriting = prop.table(table(d$monster_present,d$writing),2)
# Flip rows so hybrids are on left of plot
propHybridsByWriting = propHybridsByWriting[2:1,]
barplot(propHybridsByWriting,horiz = T,las=1)
legend(0.1,7,legend = c("Hybrids", "No Hybrids"), col=c(gray(0.1),gray(0.9)),pch=15, xpd = T,ncol = 2)
# Reset margins
par(mar=c(5, 4, 4, 2) + 0.1)

## This one needs corrections for proper viewing
par(mar=c(3,10,2,1))
propHybridsByTransport = prop.table(table(d$monster_present,d$land),2)
# Flip rows so hybrids are on left of plot
propHybridsByTransport = propHybridsByTransport[2:1,]
barplot(propHybridsByTransport,horiz = T,las=1)
legend(0.1,7,legend = c("Hybrids", "No Hybrids"), col=c(gray(0.1),gray(0.9)),pch=15, xpd = T,ncol = 2)
# Reset margins
par(mar=c(5, 4, 4, 2) + 0.1)

## This also needs corrections
par(mar=c(3,10,2,1))
propHybridsByStratification = prop.table(table(d$monster_present,d$strata),2)
# Flip rows so hybrids are on left of plot
propHybridsByStratification = propHybridsByStratification[2:1,]
barplot(propHybridsByStratification,horiz = T,las=1)
legend(0.1,7,legend = c("Hybrids", "No Hybrids"), col=c(gray(0.1),gray(0.9)),pch=15, xpd = T,ncol = 2)
# Reset margins
par(mar=c(5, 4, 4, 2) + 0.1)


par(mar=c(3,10,2,1))
propHybridsByMarket = prop.table(table(d$monster_present,d$market),2)
# Flip rows so hybrids are on left of plot
propHybridsByMarket = propHybridsByMarket[2:1,]
barplot(propHybridsByMarket,horiz = T,las=1)
legend(0.1,7,legend = c("Hybrids", "No Hybrids"), col=c(gray(0.1),gray(0.9)),pch=15, xpd = T,ncol = 2)
# Reset margins
par(mar=c(5, 4, 4, 2) + 0.1)


# Regression with the variables suggested by the decision tree

# Use (reverse) Helmert coding, so each coefficient compares the current level
#  to the previous level of the variable
contrasts(d$strata) = contr.helmert(5)
contrasts(d$land) = contr.helmert(4)
contrasts(d$writing) = contr.helmert(4)
contrasts(d$tech) = contr.helmert(4)
contrasts(d$fixity) = contr.helmert(5)

# Optional add Market
# We'll exclude cases where we don't know the status of the market
d2 = d[d$market !="Outside Local",]
d2$market = factor(d2$market,ordered=T)
# Helmert coding for Market
contrasts(d$market) = contr.helmert(4)


# Run the model - Make sure data source is correct for with or without Market
m1 = glm(factor(monster_present) ~  strata + land, 
         family="binomial", data = d)
summary(m1)
# Stats for specific variables:
car::Anova(m1)

# R-squared for model fit:
library(rsq)
rsq::rsq(m1)

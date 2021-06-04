#============================================================================
# Name: GLMs for hedgehog distribution
# Autor: Perle Charlot
# Date of creation: 14-10-2018
# Date of modification: 15-12-2018
#----------------------------------------------------------------------------
# R version: 3.5.1 
# Recquired packages: 
library(data.table)
library(PresenceAbsence)
library(car)
#----------------------------------------------------------------------------
# Aim: This script aims to test several GLMs.
# Inputs: Table observation x predictors + An independant dataset of observations
# Outputs: GLM coefficients, p-values, vif, AIC
#          Coefficients of validation (AUC, kappa).
#============================================================================

#set working directory
setwd("C:/Users/cp101159/Desktop/HEDGEHOG_R/")
rep <- getwd()
rep_data <- "/DATA/"
rep_results <- "/RESULTATS/"

#load table
Htab <- fread(paste0(rep, rep_data,'Table_100m/table100m.csv'), dec=",")
#Comments on the table: 161 025 observations (1 cell = 10 000m²= 1 ha)
#AG_cov, AL_cov, etc = % cover of land use
#MinHD, MaxHD, MHD = min, max and mean Human Density
#V_km_07.17, V_07.17 = traffic (flow and density) for 2007-2017 and 2015-2017 periods
#Dog_km2 = dog density, supposed to be quite similar for both periods
#Badger07 and Badger15 = number of badger sightings (as they are predators) for 2007-2017 and 2015-2017 periods
#Nal1km = % cover Neighbour Allotment 1 km radius (pa = park, pl=playspace,wo=woodland), 2km radius and 500m radius
#Hedgehog.07 and Hedgehog.15 = binary presence (1)/absence (0) for 2007-2017 and 2015-2017 periods
#Weight.07 and Weight.15 = rounded weight of the observation according sightings (H_rep and O_rep) for 2007-2017 and 2015-2017 periods

str(Htab)
Htab$Dog <- as.numeric(Htab$Dog)

#create 2 different dataset according to the considered period
H07<- subset(Htab, select = -c(Badger15, Hedgehog.15, Weight.15, H_rep.15,O_rep.15,V_km_15.17, V_15.17, H_rep.07, O_rep.07))
H15<- subset(Htab, select = -c(Badger07, Hedgehog.07, Weight.07, H_rep.07,O_rep.07,V_km_07.17, V_07.17, H_rep.15, O_rep.15))

#clean NA before GLM
H07.tab <- na.omit(H07)#drop from XX obs to only 122 obs because of the lack of badgers
H07.tab <- na.omit(H07)#if we remove 'badger' variable, 1042 obs remain
H07<- subset(Htab, select = -c(Badger15, Badger07,Hedgehog.15, Weight.15, H_rep.15,O_rep.15,V_km_15.17, V_15.17, H_rep.07, O_rep.07))

H15.tab <- na.omit(H15)#drop from XX obs to only 39 obs because of the lack of badgers 
H15.tab <- na.omit(H15)#if we remove 'badger' variable, 601 obs remain
H15<- subset(Htab, select = -c(Badger07, Badger15, Hedgehog.07, Weight.07, H_rep.07,O_rep.07,V_km_07.17, V_07.17, H_rep.15, O_rep.15))


######################
# Outliers Detection #
######################

H07.tab$color[H07.tab$Hedgehog.07==0] <- "red" #ABSENCE
H07.tab$color[H07.tab$Hedgehog.07==1] <- "blue" #PRESENCE

#check for every variable..
dotchart(H07.tab$AG_cov, main = "AG", color=H07.tab$color)
dotchart(H07.tab$AM_cov, main = "AM", color=H07.tab$color)
dotchart(H07.tab$AL_cov, main = "AL", color=H07.tab$color)
dotchart(H07.tab$CO_cov, main = "CO", color=H07.tab$color)
dotchart(H07.tab$PA_cov, main = "PA", color=H07.tab$color)
dotchart(H07.tab$PL_cov, main = "PL", color=H07.tab$color)
dotchart(H07.tab$WA_cov, main = "WA", color=H07.tab$color)
dotchart(H07.tab$WO_cov, main = "WO", color=H07.tab$color)
dotchart(H07.tab$V_km_07.17, main = "V_km_07.17", color=H07.tab$color)#dot at 100
dotchart(H15.tab$V_km_15.17, main = "V_km_15.17", color=H07.tab$color)
dotchart(H15.tab$V_15.17, main = "V_15.17", color=H07.tab$color)
dotchart(H07.tab$V_07.17, main = "V_07.17", color=H07.tab$color)
dotchart(H07.tab$MinHD, main = "MinHD", color=H07.tab$color)#dot at >20 000
dotchart(H07.tab$MaxHD, main = "MaxHD", color=H07.tab$color)#dot at >20 000
dotchart(H07.tab$MHD, main = "MHD", color=H07.tab$color)#dot at >20 000
dotchart(H07.tab$Dog, main = "Dog", color=H07.tab$color)
dotchart(H07.tab$Nal1km, main = "Nal1km", color=H07.tab$color)
dotchart(H07.tab$Nal2km, main = "Nal2km", color=H07.tab$color)
dotchart(H07.tab$Nal500m, main = "Nal500m", color=H07.tab$color)
dotchart(H07.tab$Npa1km, main = "Npa1km", color=H07.tab$color)
dotchart(H07.tab$Npa2km, main = "Npa2km", color=H07.tab$color)
dotchart(H07.tab$Npa500m, main = "Npa500m", color=H07.tab$color)
dotchart(H07.tab$Npl1km, main = "Npl1km", color=H07.tab$color)
dotchart(H07.tab$Npl2km, main = "Npl2km", color=H07.tab$color)
dotchart(H07.tab$Npl500m, main = "Npl500m", color=H07.tab$color)
dotchart(H07.tab$Nwo1km, main = "Nwo1km", color=H07.tab$color)
dotchart(H07.tab$Nwo2km, main = "Nwo2km", color=H07.tab$color)
dotchart(H07.tab$Nwo500m, main = "Nwo500m", color=H07.tab$color)
dotchart(H07.tab$Weight.07, main = "Weight", color=H07.tab$color)

###################
# Models Building #
###################

#2007-2017 period. We first look at collinearity (vif criterion), then significance

# MODEL 1 : ENVIRONMENTAL DATA + predator (badger) ONLY
m1.07<- glm(data = H07.tab,Hedgehog.07 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal1km+Nal2km+Nal500m+Npa1km+Npa2km+Npa500m
            +Npl1km+Npl2km+Npl500m+Nwo1km+Nwo2km+Nwo500m,
            family = binomial(link = 'logit'), weights = Weight.07)
vif(m1.07)# drop those ones > 3, and re do the glm
#Nal1km, Npa2km, Npl1km, Nwo1km, Nwo2km >3
m1bis.07<- glm(data = H07.tab,Hedgehog.07 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal2km+Nal500m+Npa2km+Npa500m
            +Npl2km+Npl500m+Nwo500m,
            family = binomial(link = 'logit'), weights = Weight.07)
vif(m1bis.07)# drop those ones > 3, and re do the glm

# MODEL 2 : SOCIO ECONOMIC DATA ONLY
m2.07 <- glm(data=H07.tab, weights = Weight.07,  family = binomial(link = 'logit'),
             formula = Hedgehog.07 ~ log(MHD) + log(MaxHD) +log(MinHD)+log(Dog)+
               log(V_km_07.17)+log(V_07.17))
vif(m2.07)# drop those ones > 3, and re do the glm
#MHD, MinHD and MaxHD >>> 3. The lowest is MinHD, so I keep only this one
m2bis.07 <- glm(data=H07.tab, weights = Weight.07,  family = binomial(link = 'logit'),
             formula = Hedgehog.07 ~ log(MaxHD)+log(Dog)+
               log(V_km_07.17)+log(V_07.17))
vif(m2bis.07)

# MODEL 3 : ALL PREDICTORS (m1 + m2)
m3.07<- glm(data = H07.tab,formula = Hedgehog.07 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal2km+Nal500m+Npa2km+Npa500m
            +Npl500m+Nwo500m+log(MaxHD)+log(Dog)+ log(V_km_07.17)+log(V_07.17),
            family = binomial(link = 'logit'), weights = Weight.07)
summary(m3.07)
vif(m3.07)# drop those ones > 3, and re do the glm

summary(m1bis.07)
summary(m2bis.07)
summary(m3.07)
#MaxHD negative but slight, Vkm positive, V negative

#2015-2017 period.
# MODEL 1 : ENVIRONMENTAL DATA + predator (badger) ONLY
m1.15<- glm(data = H15.tab,formula = Hedgehog.15 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal1km+Nal2km+Nal500m+Npa1km+Npa2km+Npa500m
            +Npl1km+Npl2km+Npl500m+Nwo1km+Nwo2km+Nwo500m
            ,family = binomial(link = 'logit'), weights = Weight.15)
vif(m1.15)#Npl1km, Npl2km, Nwo1km, Nwo2km, nWo500m
m1bis.15<- glm(data = H15.tab,formula = Hedgehog.15 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal1km+Nal2km+Nal500m+Npa1km+Npa2km+Npa500m+
            Npl500m+Nwo500m,family = binomial(link = 'logit'), weights = Weight.15)
vif(m1bis.15)

# MODEL 2 : SOCIO ECONOMIC DATA ONLY
m2.15 <- glm(data=H15.tab, weights = Weight.15,  family = binomial(link = 'logit'),
             formula = Hedgehog.15 ~ log(MHD) + log(MaxHD) +log(MinHD)+log(Dog)+
               log(V_km_15.17)+log(V_15.17))
vif(m2.15)
m2bis.15 <- glm(data=H15.tab, weights = Weight.15,  family = binomial(link = 'logit'),
             formula = Hedgehog.15 ~ log(MaxHD)+log(Dog)+log(V_km_15.17)+log(V_15.17))
vif(m2bis.15)

# MODEL 3 : ALL PREDICTORS (m1 + m2)
m3.15<- glm(data = H15.tab,formula = Hedgehog.15 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal1km+Nal2km+Nal500m+Npa2km+Npa500m
            +Npl2km+Npl500m+Nwo2km+Nwo500m
              +log(MaxHD)+log(Dog)+log(V_km_15.17)+log(V_15.17),
            family = binomial(link = 'logit'), weights = Weight.15)
vif(m3.15)# drop those ones > 3, and re do the glm

summary(m1bis.15)
summary(m2bis.15)
summary(m3.15)
#here, Vkm negative and V strong positive

#####################
# Models Comparison #
#####################

summ.table.07 <- do.call(rbind, lapply(list(m1bis.07,m2bis.07,m3.07), broom::glance))
table.cols.07 <- c("df.residual", "deviance", "AIC")
reported.table.07 <- summ.table.07[table.cols.07]

summ.table.15 <- do.call(rbind, lapply(list(m1bis.15,m2bis.15,m3.15), broom::glance))
table.cols.15 <- c("df.residual", "deviance", "AIC")
reported.table.15 <- summ.table.15[table.cols.15]

reported.table.07
reported.table.15
#for both periods, best model is 3 (env+socio eco), then it's ENV

######################
# Models Predictions #
######################

#2007-2017: 10 cross validation method 
train_index <- matrix(nrow = round(0.7*length(H07.tab$AG_cov)), ncol=10)
test_index<-matrix(nrow = round(0.3*length(H07.tab$AG_cov)), ncol=10)
predm1 <- test_index
predm2 <- test_index
predm3 <- test_index
observed_test <- test_index
ID_test <- test_index
for (i in 1:10){
  train_index[,i] <- sample(1:length(H07.tab$AG_cov), round(0.7*length(H07.tab$AG_cov)))
  test_index[,i] <- setdiff(1:length(H07.tab$AG_cov), train_index[,i])
  #Build training and testing datasets
  train <- H07.tab[train_index[,i],]
  test <- H07.tab[test_index[,i],]
  test <- test[order(test[,1]),]
  m1 <- glm(data = train,Hedgehog.07 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal2km+Nal500m+Npa2km+Npa500m
            +Npl2km+Npl500m+Nwo500m,
            family = binomial(link = 'logit'), weights = Weight.07)
  m2 <- glm(data=train, weights = Weight.07,  family = binomial(link = 'logit'),
            formula = Hedgehog.07 ~ log(MaxHD)+log(Dog)+
              log(V_km_07.17)+log(V_07.17))
  m3 <- glm(data = train,formula = Hedgehog.07 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal2km+Nal500m+Npa2km+Npa500m
            +Npl500m+Nwo500m+log(MaxHD)+log(Dog)+ log(V_km_07.17)+log(V_07.17),
            family = binomial(link = 'logit'), weights = Weight.07)
  predm1[,i] <- as.numeric(predict.glm(m1, test, type=c("response")))
  predm2[,i] <- as.numeric(predict.glm(m2, test, type=c("response")))
  predm3[,i] <- as.numeric(predict.glm(m3, test, type=c("response")))
  observed_test[,i] <- test$Hedgehog.07
  ID_test[,i] <- test$id}
DATA_test <- cbind(c(ID_test[]), c(observed_test[]), c(predm1[]),c(predm2[]), c(predm3[]))
ind <-which(duplicated(DATA_test[,1]))
DATA_test.07 <- DATA_test[-ind,]#1130 obs

#Plot AUC, sensitivity, specificity, kappa
auc.roc.plot(DATA_test.07, which.model = c(1:3), color = TRUE, opt.thresholds = TRUE, add.legend=F,
             model.names = c('m1','m2','m3'), main = 'Splitting 70/30 x10 Fox/Badger,2007-2017',opt.methods = c('MaxKappa'))
auc(DATA_test, which.model = c(1))#red
auc(DATA_test, which.model = c(2))#green
auc(DATA_test, which.model = c(3))#blue
error.threshold.plot(DATA_test, which.model = 1, color = TRUE, add.legend = FALSE, main="m1, Splitting 70/30 x10 Fox/Badger,2007-2017")
error.threshold.plot(DATA_test, which.model = 2, color = TRUE, add.legend = FALSE, main="m2, Splitting 70/30 x10 Fox/Badger,2007-2017")
error.threshold.plot(DATA_test, which.model = 3, color = TRUE, add.legend = FALSE, main="m3.07, Splitting 70/30 x10 Fox/Badger,2007-2017")

#2015-2017: 10 cross validation method 
train_index <- matrix(nrow = round(0.7*length(H15.tab$AG_cov)), ncol=10)
test_index<-matrix(nrow = round(0.3*length(H15.tab$AG_cov)), ncol=10)
predm1 <- test_index
predm2 <- test_index
predm3 <- test_index
observed_test <- test_index
ID_test <- test_index
for (i in 1:10){
  train_index[,i] <- sample(1:length(H15.tab$AG_cov), round(0.7*length(H15.tab$AG_cov)))
  test_index[,i] <- setdiff(1:length(H15.tab$AG_cov), train_index[,i])
  #Build training and testing datasets
  train <- H15.tab[train_index[,i],]
  test <- H15.tab[test_index[,i],]
  test <- test[order(test[,1]),]
  m1 <- glm(data = train,formula = Hedgehog.15 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal1km+Nal2km+Nal500m+Npa1km+Npa2km+Npa500m+
              Npl500m+Nwo500m,family = binomial(link = 'logit'), weights = Weight.15)
  m2 <- glm(data=train, weights = Weight.15,  family = binomial(link = 'logit'),
            formula = Hedgehog.15 ~ log(MaxHD)+log(Dog)+log(V_km_15.17)+log(V_15.17))
  m3 <- glm(data = train,formula = Hedgehog.15 ~ AG_cov + WA_cov + WO_cov+CO_cov+PL_cov+
              PA_cov+AM_cov+AL_cov+OT_cov+Nal1km+Nal2km+Nal500m+Npa2km+Npa500m
            +Npl2km+Npl500m+Nwo2km+Nwo500m+log(MaxHD)+log(Dog)+log(V_km_15.17)+log(V_15.17),
            family = binomial(link = 'logit'), weights = Weight.15)
  predm1[,i] <- as.numeric(predict.glm(m1, test, type=c("response")))
  predm2[,i] <- as.numeric(predict.glm(m2, test, type=c("response")))
  predm3[,i] <- as.numeric(predict.glm(m3, test, type=c("response")))
  observed_test[,i] <- test$Hedgehog.15
  ID_test[,i] <- test$id}
DATA_test.15 <- cbind(c(ID_test[]), c(observed_test[]), c(predm1[]),c(predm2[]), c(predm3[]))
ind <-which(duplicated(DATA_test.15[,1]))
DATA_test.15 <- DATA_test.15[-ind,]#658obs

#Plot AUC, sensitivity, specificity, kappa
auc.roc.plot(DATA_test.15, which.model = c(1:3), color = TRUE, opt.thresholds = TRUE, add.legend=F,
             model.names = c('m1','m2','m3'), main = 'Splitting 70/30 x10 Fox/Badger,2015-2017',opt.methods = c('MaxKappa'))
auc(DATA_test.15, which.model = c(1))#red
auc(DATA_test.15, which.model = c(2))#green
auc(DATA_test.15, which.model = c(3))#blue
error.threshold.plot(DATA_test.15, which.model = 1, color = TRUE, add.legend = FALSE, main="m1, Splitting 70/30 x10 Fox/Badger,2015-2017")
error.threshold.plot(DATA_test.15, which.model = 2, color = TRUE, add.legend = FALSE, main="m2, Splitting 70/30 x10 Fox/Badger,2015-2017")
error.threshold.plot(DATA_test.15, which.model = 3, color = TRUE, add.legend = FALSE, main="m3.15, Splitting 70/30 x10 Fox/Badger,2015-2017")


###########
# Mapping #
###########

#Run GLM for all across Greater London map(>160 000 cells)
preda <- as.numeric(predict.glm(m3.07, Htab, type=c("response")))
predb <- as.numeric(predict.glm(m3.15, Htab, type=c("response")))
obs.07 <- Htab$Hedgehog.07
obs.15 <- Htab$Hedgehog.15
ID <- Htab$id
DATAmap <- cbind(ID,obs.07,preda,obs.15,predb)
head(DATAmap)

#Chose threshold according to AUC roc plot
#max kappa
threA = 0.45 #m1
threB = 0.45 #m2
map = DATAmap 
#transformation probability in binary absence/presence, model A
predi.A = c()
for(i in 1:length(map[,3])){
  prob = as.numeric(map[i,3])
  if(is.na(prob) == F){
    if(prob >= threA){predi.A[i]=1}
    else{predi.A[i]=0}}}
#transformation probability in binary absence/presence, model B
predi.B = c()
for(i in 1:length(map[,5])){
  prob = as.numeric(map[i,5])
  if(is.na(prob)==F){
    if(prob >= threB){predi.B[i]=1}
    else{predi.B[i]=0}}}

map <- cbind(map[,1],map[,2],predi.A,map[,4],predi.B)
colnames(map) =c('id','obs.07','m3.07','obs.15','m3.15')
map = as.data.frame(map)
map$diff = map[,3]+map[,5]
head(map)

#export prediction to map it with QGIS
write.csv(map, file = paste0(rep, rep_results,"tables/binary_prediction_100m.csv"))
#highlight similarities and differences
colnames(DATAmap) = c('id', 'obs07','m3.07','obs.15','m3.15')
write.csv(DATAmap, file = paste0(rep, rep_results,"proba_prediction_100m.csv"))
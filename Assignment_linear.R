
#Reading the file and storing in carprice df
carprice <- read.csv("CarPrice_Assignment.csv")

#checking the structure,na,unique and duplicate values
str(carprice)
sum(is.na(carprice)) #no na
duplicated(carprice) # no duplicates
length(unique(carprice)) 


library(tidyr)
#splitting car name into brand and model
carprice_edit <- separate(carprice, CarName, into=c("CompanyBrand","ModelName"),sep = " ",extra = "merge", fill = "right")
carprice_edit <- carprice_edit[,-4]  #removing model name as per req         
str(carprice_edit)
#converting the company car name into factor
carprice_edit$CompanyBrand <- as.factor(carprice_edit$CompanyBrand)
#checking for different brand names
unique(carprice_edit$CompanyBrand)

#we found few mistakes with the names so replacing them with the correct ones
library(stringr)
carprice_edit$CompanyBrand <- str_replace_all(carprice_edit$CompanyBrand,"maxda","mazda")
carprice_edit$CompanyBrand <- str_replace_all(carprice_edit$CompanyBrand,"vokswagen","volkswagen")
carprice_edit$CompanyBrand <- str_replace_all(carprice_edit$CompanyBrand,"toyouta","toyota")
carprice_edit$CompanyBrand <- str_replace_all(carprice_edit$CompanyBrand,"Nissan","nissan")
carprice_edit$CompanyBrand <- str_replace_all(carprice_edit$CompanyBrand,"vw","volkswagen")
carprice_edit$CompanyBrand <- str_replace_all(carprice_edit$CompanyBrand,"porcshce","porsche")

carprice_edit$CompanyBrand <- as.factor(carprice_edit$CompanyBrand)
#Convert into categorical per req
carprice_edit$symboling <- as.factor(carprice_edit$symboling)

summary(carprice_edit)

#Checking for outliers and replacing them with the border values
quantile(carprice_edit$wheelbase,seq(0,1,0.01))
carprice_edit$wheelbase[which(carprice_edit$wheelbase>115.544)]<-115.544

quantile(carprice_edit$carlength,seq(0,1,0.01))
carprice_edit$carlength[which(carprice_edit$carlength>202.480)]<-202.480
carprice_edit$carlength[which(carprice_edit$carlength<155.900)]<-155.900

quantile(carprice_edit$carwidth,seq(0,1,0.01))
boxplot(carprice_edit$carwidth)
carprice_edit$carwidth[which(carprice_edit$carwidth<62.536)]<-62.536
carprice_edit$carwidth[which(carprice_edit$carwidth>71.700)]<-71.700

quantile(carprice_edit$carheight,seq(0,1,0.01))

quantile(carprice_edit$curbweight,seq(0,1,0.01))
carprice_edit$curbweight[which(carprice_edit$curbweight<1819.72)]<-1819.72
carprice_edit$curbweight[which(carprice_edit$curbweight>3503.00)]<-3503.00

quantile(carprice_edit$enginesize,seq(0,1,0.01))
carprice_edit$enginesize[which(carprice_edit$enginesize>209.00)]<-209.00

quantile(carprice_edit$boreratio,seq(0,1,0.01))
carprice_edit$boreratio[which(carprice_edit$boreratio<2.9100)]<-2.9100

quantile(carprice_edit$stroke,seq(0,1,0.01))
carprice_edit$stroke[which(carprice_edit$stroke<2.6400)]<-2.6400
carprice_edit$stroke[which(carprice_edit$stroke>3.6400)]<-3.6400


quantile(carprice_edit$compressionratio,seq(0,1,0.01))
carprice_edit$compressionratio[which(carprice_edit$compressionratio>10.0000)]<-10.0000

quantile(carprice_edit$horsepower,seq(0,1,0.01))
carprice_edit$horsepower[which(carprice_edit$horsepower>184.00)]<-184.00 
boxplot(carprice_edit$horsepower)

quantile(carprice_edit$peakrpm,seq(0,1,0.01))
carprice_edit$peakrpm[which(carprice_edit$peakrpm>6000)]<-6000

quantile(carprice_edit$citympg,seq(0,1,0.01))
carprice_edit$citympg[which(carprice_edit$citympg>38.00)]<-38.00

quantile(carprice_edit$highwaympg,seq(0,1,0.01))
carprice_edit$highwaympg[which(carprice_edit$highwaympg>46.92)]<-46.92


summary(carprice_edit)
str(carprice_edit)

#Converting the 2 factor/categories to 0's and 1's 
levels(carprice_edit$fueltype)<-c(1,0)
carprice_edit$fueltype <- as.numeric(levels(carprice_edit$fueltype))[carprice_edit$fueltype]
summary(carprice_edit$fueltype)

levels(carprice_edit$aspiration)<-c(1,0)
carprice_edit$aspiration <- as.numeric(levels(carprice_edit$aspiration))[carprice_edit$aspiration]

levels(carprice_edit$doornumber)<-c(1,0)
carprice_edit$doornumber <- as.numeric(levels(carprice_edit$doornumber))[carprice_edit$doornumber]

levels(carprice_edit$enginelocation)<-c(1,0)
carprice_edit$enginelocation <- as.numeric(levels(carprice_edit$enginelocation))[carprice_edit$enginelocation]

summary(carprice_edit)
str(carprice_edit)

#Creating dummy variables
#grouping symboling to 3 categories. Considering -3,2 as safe, -1,0,1 as moderate and 2,3 as risky
#Symboling -3,-2,-1,0,1,2,3
summary(factor(carprice_edit$symboling))
#We can see -3 is not present in the data, so considering -2 for safe
levels(carprice_edit$symboling)[1] <- "safe"
levels(carprice_edit$symboling)[2:4] <- "moderate"
levels(carprice_edit$symboling)[3:4] <- "risky"

#Creating dummy variables for variables more than 2 factors 
dummy1 <- data.frame(model.matrix(~symboling-1,data=carprice_edit))
View(dummy1)
#deleting a variable as we consider n-1 columns
dummy1 <- dummy1[,-1]

dummy2 <- data.frame(model.matrix(~CompanyBrand-1,data=carprice_edit))
View(dummy2)
dummy2 <- dummy2[,-1]

dummy3 <- data.frame(model.matrix(~carbody-1,data=carprice_edit))
View(dummy3)
dummy3 <- dummy3[,-1]

dummy4 <- data.frame(model.matrix(~drivewheel-1,data=carprice_edit))
View(dummy4)
dummy4 <- dummy4[,-1]

dummy5 <- data.frame(model.matrix(~enginetype-1,data=carprice_edit))
View(dummy5)
dummy5 <- dummy5[,-1]

dummy6 <- data.frame(model.matrix(~cylindernumber-1,data=carprice_edit))
View(dummy6)
dummy6 <- dummy6[,-1]

dummy7 <- data.frame(model.matrix(~fuelsystem-1,data=carprice_edit))
View(dummy7)
dummy7 <- dummy7[,-1]

#all the categorical are now converted to numeric
#Adding the created variables to the main dataset in place of the existing categorical variables
carprice_edit1 <- cbind(carprice_edit[,-c(2,3,7,8,15,16,18)],dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#removing car id
carprice_edit1 <- carprice_edit1[,-1]
str(carprice_edit1)
summary(carprice_edit1)        

##############

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice_edit1), 0.7*nrow(carprice_edit1))
# generate the train data set
train = carprice_edit1[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice_edit1[-trainindices,]

#verifying the structure of train
str(train)

#Creating our first model. As we have to predict price which is the dependent variable 
#using our independent variables
model_1 <- lm(price~.,data=train)
summary(model_1) #0.9626
#as we can see the model has NA values and has highly insignificant variables.
#Let us use the stepAIC function to remove higly insignificant variables

library(MASS)

step <- stepAIC(model_1,direction="both")
step

#lets use the model derived from stepaic
#we can see lot of insignificant independent variables are removed after the above process
model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + stroke + horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandtoyota + CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)

#checking the summary of the model_2
summary(model_2)#0.9677
#the adjusted r square is good enough but the model still has insignificant variables making the
#model quite unstable.

# Let us use VIF to check multi collinearity
library(car)
vif(model_2)
#We can see the model having high vif values. Let us try to remove variables having high vif and high p values 
#Almost all the high vif variables are significant. lets check the correltion among them
c1<-data.frame(train$cylindernumberfour,train$carbodysedan,train$curbweight)
cor(c1)
#There is high correlation between carbosysedan and cylindernumberfour.
#Removing carbodysedan as it is having lower significane value
model_3 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + stroke + horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandtoyota + CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train)
#the model is still unstable
summary(model_3) #0.9663
vif(model_3) #high vif values still prevail

#Clearly collinearity exists, lets find out the correlation among them
c2<-data.frame(train$cylindernumberfour,train$cylindernumbersix,train$curbweight)
cor(c2)
#There is a strong relationship between cylinndernumbersix and cylindernumberfour
#removing cylindernumbersix as it has lower significance level and rebuilding the model
model_4 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + stroke + horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandtoyota + CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + cylindernumberfour
            , data = train)
# Thre is a minute change in adjusted r square which is a good sign but the model is yet not stable
summary(model_4) #0.9603
vif(model_4)  # but the vif values dropped showing a strong collinearity
#Yet there are high vif values and are significant.

#checking the relationship again
c3<-data.frame(train$horsepower,train$carwidth,train$curbweight)
cor(c3)
# based on the correlation and significance value we are removing carwidth
model_5 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                 curbweight + stroke + horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandtoyota + CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + cylindernumberfour
              , data = train)
#There is a minute change in the adjusted r square value  which is a good sign.
# The model still has insignificant independent variables.
summary(model_5) #0.9534
vif(model_5)

cor(train$curbweight,train$horsepower)
#Checking the correlation and removing curbweight as per lower significance level
model_6 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                stroke + horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandtoyota + CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + cylindernumberfour
              , data = train)

summary(model_6) #0.9417
vif(model_6) #the vif values are decreased but are still >2

#removing drivewheelrwd as it has high vif and is insignificant in the model
model_7 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                stroke + horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandtoyota + CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour
              , data = train)
#model still has insignificant variables.
summary(model_7) #0.9404
vif(model_7)

c4<-data.frame(train$cylindernumberfour,train$wheelbase,train$CompanyBrandtoyota)
cor(c4)

#There is a strong positive relation between CompanyBrandtoyota and cylindernumberfour
#removing CompanyBrandtoyota keeping in view the lower significance value
model_8 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                stroke + horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                enginetyperotor + cylindernumberfive + cylindernumberfour
              , data = train)

vif(model_8)
summary(model_8) #0.918

#removing stroke as it has high vif and not significant (p>0.05)

model_9 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                 horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                enginetyperotor + cylindernumberfive + cylindernumberfour
              , data = train)
vif(model_9)
summary(model_9) #0.9166
#cylindernumberfour has high vif value, let us try to remove it and check if the adjusted r value is impacting
#removing cylindernumberfour
model_10 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                horsepower + CompanyBrandbmw + 
                CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                CompanyBrandvolkswagen + CompanyBrandvolvo + 
                carbodyhardtop + carbodyhatchback +  carbodywagon + 
                enginetyperotor + cylindernumberfive 
              , data = train)
#there has been impact imterms of redution of vif's and the adjusted r square value hasn't been changed much 
summary(model_10) #0.9106
vif(model_10)
#removing wheelbase on the basis of high vif and p >0.05
model_11 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick + CompanyBrandchevrolet + CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetyperotor + cylindernumberfive 
               , data = train)
vif(model_11)
summary(model_11) #0.908
#all the vif are normal and removing the variables based on their significance levels.
#removing CompanyBrandchevrolet as it is higly insignificant among the variables in the latest model

model_12 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandrenault + CompanyBrandsaab + CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetyperotor + cylindernumberfive 
               , data = train)
vif(model_12)
summary(model_12) #0.9088

#removing CompanyBrandrenault as p>0.05
model_13 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                  CompanyBrandsaab + CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetyperotor + cylindernumberfive 
               , data = train)
vif(model_13)
summary(model_13) #0.9095

#removing cylindernumberfive as p > 0.05
model_14 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmazda + CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsaab + CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetyperotor 
               , data = train)
vif(model_14)
summary(model_14) #0.9101

#removing CompanyBrandmazda as p>0.05 and high among others
model_15 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                  CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsaab + CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon + 
                 enginetyperotor 
               , data = train)
vif(model_15)
summary(model_15) #0.9107

#removing enginetyperotor as p value is high
model_16 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsaab + CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon 
               , data = train)
vif(model_16)
summary(model_16) #0.9112

# removing CompanyBrandsaab as p is insignificant
model_17 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                  CompanyBrandsubaru + 
                 CompanyBrandvolkswagen + CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon 
               , data = train)
vif(model_17)
summary(model_17) #0.9116

#CompanyBrandvolkswagen as p is not significant
model_18 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                  CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback +  carbodywagon 
               , data = train)
vif(model_18)
summary(model_18) #0.912

# here it is carbodywagon, the p value is >.05
model_19 <- lm(formula = price ~ aspiration + enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback  
               , data = train)
vif(model_19)
summary(model_19) #0.9121

#removing aspiration as it is insignificant
model_20 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda + CompanyBrandisuzu + CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback  
               , data = train)
vif(model_20)
summary(model_20) #0.9122

#CompanyBrandisuzu p >0.05
model_21 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandhonda +  CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback  
               , data = train)
vif(model_21)
summary(model_21) #0.912

# removing CompanyBrandhonda based on p value
model_22 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                   CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                 CompanyBrandnissan + CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback  
               , data = train)
vif(model_22)
summary(model_22) #0.9115

#removing CompanyBrandnissan based on p value
model_23 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandjaguar + 
                 CompanyBrandmercury + CompanyBrandmitsubishi + 
                  CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback  
               , data = train)
vif(model_23)
summary(model_23) #0.9112

#removing CompanyBrandmercury, as p is 0.157530 > 0.05
model_24 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandjaguar + 
                  CompanyBrandmitsubishi + 
                 CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo + 
                 carbodyhardtop + carbodyhatchback  
               , data = train)
vif(model_24)
summary(model_24) #0.9105


#removing carbodyhardtop based on p value
model_25 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandjaguar + 
                 CompanyBrandmitsubishi + 
                 CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo + 
                  carbodyhatchback  
               , data = train)
vif(model_25)
summary(model_25) #0.9096

#removing carbodyhatchback based on p value
model_26 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandjaguar + 
                 CompanyBrandmitsubishi + 
                 CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandsubaru + 
                 CompanyBrandvolvo  
               , data = train)
vif(model_26)
summary(model_26) #0.9086

#removing CompanyBrandsubaru as p >0.05
model_27 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandjaguar + 
                 CompanyBrandmitsubishi + 
                 CompanyBrandpeugeot + CompanyBrandplymouth + 
                 CompanyBrandvolvo  
               , data = train)
vif(model_27)
summary(model_27) #0.9077

#removing CompanyBrandplymouth based on p and it moderately significant
model_28 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick +  CompanyBranddodge + 
                 CompanyBrandjaguar + 
                 CompanyBrandmitsubishi + 
                 CompanyBrandpeugeot  + 
                 CompanyBrandvolvo  
               , data = train)
vif(model_28)
summary(model_28) #0.9057
#this didnt affect the r value, removing CompanyBrandplymouth is not a problem

#removing CompanyBranddodge based on p and it moderately significant
model_29 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick  + 
                 CompanyBrandjaguar + 
                 CompanyBrandmitsubishi + 
                 CompanyBrandpeugeot  + 
                 CompanyBrandvolvo  
               , data = train)
vif(model_29)
summary(model_29) #0.9041
#this didnt affect the r value, removing CompanyBrandplymouth is not a problem

#removing CompanyBrandmitsubishi based on p and it moderately significant
model_30 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick  + 
                 CompanyBrandjaguar + 
                 CompanyBrandpeugeot  + 
                 CompanyBrandvolvo  
               , data = train)
vif(model_30)
summary(model_30) #0.9013
#this didnt affect the r value, removing CompanyBrandplymouth is not a problem

#removing CompanyBrandvolvo based on p and it moderately significant
model_31 <- lm(formula = price ~  enginelocation +  
                 horsepower + CompanyBrandbmw + 
                 CompanyBrandbuick  + 
                 CompanyBrandjaguar + 
                 CompanyBrandpeugeot   
               , data = train)
vif(model_31)
summary(model_31) #0.894
#Now the model is highly significant with six independent variables in place.


#Lets predict the price based on the model we have created.
Predict<-predict(model_31, test[, -18])
test$test_price <- Predict #the predicted values are in the column test_price of test dataset. 

# let us check the accuracy of the predictions
# calculate R squared by squaring correlation between price and the predicted price.
rsquared <- cor(test$price,test$test_price)^2
# check R-squared
rsquared #0.8068

#the correlation obtained is strong and positive which tell us the model created is good enough.

#Train : Multiple R-squared:  0.8985,	Adjusted R-squared:  0.894
#rsquared of test : 0.8068

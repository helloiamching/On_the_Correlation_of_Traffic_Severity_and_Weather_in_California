stationary_cal<- read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/stationary_cal.csv")
tehama = read.csv('/Users/wang.c/Desktop/112-1 NCCU/lfd/Tehama.csv') 
train <- read.csv("/Users/wang.c/Desktop/112-1 NCCU /lfd/model training data/x_train.csv")
test <-read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/model training data/x_test.csv")
train <- subset(train, select = -c(Severity))
test <- subset(test, select = -c(Severity))
Tehama_train <- read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/model training data/Tehama_train.csv")
Tehama_test <-read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/model training data/Tehama_test.csv")

library(tidyverse)  # Modern data science workflow
library(MASS)
library(sf)
library(sp)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(ggthemes)
library(GWmodel)
library(sjPlot)
library(tableone)

df.Tehama = read.csv('/Users/wang.c/Desktop/112-1 NCCU/lfd/Tehama.csv')
df.Tehama$Severity = factor(df.Tehama$Severity,levels = c(1,2,3,4))

#train and test set
library("caTools")
split = sample.split(sf.Tehama$Severity2,SplitRatio = .8)
train_data = subset(sf.Tehama,split == TRUE)
test_data  = subset(sf.Tehama,split == FALSE)


# Check the column names in the data frame
colnames(stationary_cal)

# Adjust the tab_corr call with the correct column names
tab_corr(data = stationary_cal[c('Severity', 'Distance.mi.', 'Temperature.F.', 'Wind_Chill.F.', 'Humidity...', 'Pressure.in.', 'Visibility.mi.', 'Wind_Speed.mph.', 'Lasting_time')]
         ,'pairwise',"spearman",triangle = 'upper',p.numeric = T,val.rm = 0.0001)

kruskal.test(Temperature.F.~Severity2,data = stationary_cal)
kruskal.test(Pressure.in.~Severity2,data = stationary_cal)

(stationary_cal$Temperature.F.)[stationary_cal$Severity2 ==0]

wilcox.test(x = (stationary_cal$Temperature.F.)[stationary_cal$Severity2 ==0],
            y = (stationary_cal$Temperature.F.)[stationary_cal$Severity2 ==1])

wilcox.test(x = (stationary_cal$Distance.mi.)[stationary_cal$Severity2 ==0],
            y = (stationary_cal$Distance.mi.)[stationary_cal$Severity2 ==1])

boxplot(Temperature.F.~Severity,data = stationary_cal)

?wilcox.test

sig.count = 0
for(i in 1:1000){
  if(cor.test(runif(10000),runif(10000))$p.value<0.05){
    sig.count = sig.count+1
  }
}

colnames(stationary_cal)

cor.test(stationary_cal$Severity,stationary_cal$Pressure.in.,method = 'kendall')

stationary_cal = na.omit(stationary_cal)


xy = df.Tehama[c("Start_Lat","Start_Lng")]

sf.Tehama = SpatialPointsDataFrame(coords = xy, data = df.Tehama,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
xy = Tehama_train[c("Start_Lat","Start_Lng")]

sf.Tehama_train = SpatialPointsDataFrame(coords = xy, data = Tehama_train,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
xy = Tehama_test[c("Start_Lat","Start_Lng")]
sf.Tehama_test = SpatialPointsDataFrame(coords = xy, data = Tehama_test,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

colnames(sf.Tehama)
####################
null.model = glm(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,data =  df.Tehama,family = binomial(link = "logit"))

null.model = polr(Severity~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,data =  df.Tehama,Hess = T)

summary(null.model)

(ctable <- coef(summary(null.model)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

null.model$c
###################
# Create a logical vector to identify complete cases in selected columns
complete_cases <- complete.cases(stationary_cal[c("Distance.mi.", "Temperature.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Lasting_time", "Severity")])

# Subset the data frame to keep only the complete cases
stationary_cal <- stationary_cal[complete_cases, ]

stationary_cal$Severity <- as.factor(stationary_cal$Severity)

big.model = polr(Severity~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,data = stationary_cal,Hess = T)

big.model
summary(big.model)
(ctable <- coef(summary(big.model)))
####model used (only Tehama)
train <- train[complete_cases, ]

train$Severity <- as.factor(train$Severity)

big.model_train = polr(Severity~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,data = train,Hess = T)

big.model_train
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

###################

gwr.model.selection(DeVar = Severity2,c(Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time),
                    data = sf.Tehama,bw = bw1) 



bw1 = ggwr.sel(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time
               ,data =  df.Tehama , cbind(df.Tehama$Start_Lng,df.Tehama$Start_Lat),gweight = gwr.Gauss,
          family = binomial,longlat = TRUE, tol=.Machine$double.eps^0.25)

bw1 = bw.ggwr(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,
              data = sf.Tehama,family = 'binomial',kernel = 'gaussian',longlat = T)
#ggwr??
model1 = ggwr(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time
              ,data =  df.Tehama , cbind(df.Tehama$Start_Lng,df.Tehama$Start_Lat),bandwidth=bw1,gweight = gwr.Gauss,
          family = binomial(link = "logit"),longlat = TRUE,type = "pearson")

model2 = ggwr.basic(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,
                    data = sf.Tehama,family = 'binomial',kernel = 'gaussian',bw=0.9542603)
plot(x = 1:length(model2$SDF$Distance.mi.),
     (model2$SDF$Distance.mi.-mean(model2$SDF$Distance.mi.))/sd(model2$SDF$Distance.mi.),
     xaxs="i",yaxs="i",xaxt="n",yaxt="n", xlab = 'hellp', ylab = 'hellp')
title ()
model2
residuals(model1)

sf = st_as_sf(model1$SDF)

sf$geometry

ggplot() + geom_sf(data=tehama)+geom_sf(data = sf) +
  coord_sf() +
  theme_map() +
  scale_color_gradient(low="blue", high="red")+
  ggtitle(paste("Residuals"))

ggplot(data = df.Tehama,aes(x = Start_Lng,y = Start_Lat))+
  geom_point(color =factor(model1$SDF$Lasting_time))


ggplot(sf$Lasting_time,aes(x =df.Tehama$Start_Lng,y = df.Tehama$Start_Lat))

sf$Lasting_time

LMZ.F2GWR.test(model1)

colnames(df.Tehama)

lm(df.Tehama$Distance.mi.~df$Humidity...)
print(5)

install.packages("raster",dependencies = TRUE)
install.packages("rgdal",dependencies = TRUE)
install.packages("rgeos",dependencies = TRUE)
install.packages("sp",dependencies = TRUE)


library(sp)

### evaluate the fitting model
#p-value, r adjusted square, confusion matrix, kappa 
# Extract p-values
# Load the MASS package if not already loaded
library(MASS)
big.model_train <- polr(Severity ~ Distance.mi. + Temperature.F. + Humidity... + Pressure.in. + Visibility.mi. + Lasting_time, data = train, Hess = TRUE)
coefficients <- coef(big.model_train)
standard_errors <- summary(big.model_train)$coefficients[, "Std. Error"]
z_values <- coefficients / standard_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
results <- data.frame(Coefficient = coefficients, "Std. Error" = standard_errors, "Z Value" = z_values, "P-Value" = p_values)
print(results)

Tehama_model_train <- polr(Severity ~ Distance.mi. + Temperature.F. + Humidity... + Pressure.in. + Visibility.mi. + Lasting_time, data = Tehama_train, Hess = TRUE)
coefficients <- coef(Tehama_model_train)
standard_errors <- summary(Tehama_model_train)$coefficients[1:6, "Std. Error"]
z_values <- coefficients / standard_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
results <- data.frame(Coefficient = coefficients, "Std. Error" = standard_errors, "Z Value" = z_values, "P-Value" = p_values)
print(results)


#Import required library
library(caret)
library(nnet)
# Make sure 'Severity' in test data is a factor
Tehama_train <- Tehama_train[complete_cases, ]
Tehama_train$Severity <- as.factor(Tehama_train$Severity)
Tehama_model_train = polr(Severity~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,data = Tehama_train,Hess = T)
Tehama_model_train

predictions <- predict(Tehama_model_train, newdata = Tehama_test)
confusion_matrix <- table(predictions, Tehama_test$Severity)
print(confusion_matrix)

# Calculate Kappa
kappa_value <- kappa(confusion_matrix)

# Assuming big.model_train is your ordered logistic regression model
library(pscl)
pR2_adjusted <- pR2adjust(big.model_train)

# Fit your ordered logistic regression model
library(MASS)  # Load the required package
Tehama_model_train <- polr(Severity ~ Distance.mi. + Temperature.F. + Humidity... + Pressure.in. + Visibility.mi. + Lasting_time, data = Tehama_test, Hess = TRUE)
AIC_full <- AIC(Tehama_model_train)
null_model <- polr(Severity ~ 1, data = Tehama_test, Hess = TRUE)
AIC_null <- AIC(null_model)
adjusted_R_squared <- 1 - (AIC_full / AIC_null)


# Display the results
cat("P-values:\n")
print(p_values)
cat("\nConfusion Matrix:\n")
print(confusion_matrix)
cat("\nAdjusted Rsquare:\n")
print(adjusted_R_squared)
cat("\nKappa:\n")
print(kappa_value)


library(sp)

### evaluate the fitting model
#p-value, r adjusted square, confusion matrix, kappa 
# Extract p-values
# Load the MASS package if not already loaded
library(MASS)
model1_train <- polr(Severity ~ Distance.mi. + Temperature.F. + Humidity... + Pressure.in. + Visibility.mi. + Lasting_time, data = train, Hess = TRUE)
coefficients <- coef(model1_train)
standard_errors <- summary(model1_train)$coefficients[, "Std. Error"]
z_values <- coefficients / standard_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
results <- data.frame(Coefficient = coefficients, "Std. Error" = standard_errors, "Z Value" = z_values, "P-Value" = p_values)
print(results)




#Import required library
library(caret)
library(nnet)
# Make sure 'Severity' in test data is a factor
predictions <- predict(model2_train, newdata = sf.Tehama_test)
bw1 = bw.ggwr(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,
              data = sf.Tehama_train ,family = 'binomial',kernel = 'gaussian',longlat = T)

model2_train = ggwr.basic(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time,
                    data = sf.Tehama_train, regression.points = sf.Tehama_test ,family = 'binomial',kernel = 'gaussian',bw=0.9542603)

confusion_matrix <- table(model2_train$glms$y, sf.Tehama_test$Severity2)
print(confusion_matrix)

# Calculate Kappa
kappa_value <- kappa(confusion_matrix)

# Assuming big.model_train is your ordered logistic regression model
library(pscl)
pR2_adjusted <- pR2adjust(model1)

# Fit your ordered logistic regression model
library(MASS)  # Load the required package
model1_train = ggwr(Severity2~Distance.mi.+Temperature.F.+Humidity...+Pressure.in.+Visibility.mi.+Lasting_time
                    ,data =  train , cbind(df.Tehama$Start_Lng,df.Tehama$Start_Lat),bandwidth=bw1,gweight = gwr.Gauss,
                    family = binomial(link = "logit"),longlat = TRUE,type = "pearson")
AIC_full <- AIC(model1_train)
null_model <- polr(Severity ~ 1, data = train, Hess = TRUE)
AIC_null <- AIC(null_model)
adjusted_R_squared <- 1 - (AIC_full / AIC_null)


# Display the results
cat("P-values:\n")
print(p_values)
cat("\nConfusion Matrix:\n")
print(confusion_matrix)
cat("\nAdjusted Rsquare:\n")
print(adjusted_R_squared)
cat("\nKappa:\n")
print(kappa_value)



###切資料集的時候要考慮unbalance
#每一次都要給資料結構
#logistic regression 要切法可以1|23, 12|3->自己寫
#auto regression

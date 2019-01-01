library(readxl)
install.packages("caTools")
library(caTools)  

kc_house_data <- read_excel("C:/Users/sneha/Downloads/kc_house_data.xlsx")
View(kc_house_data)
hs<-kc_house_data
hs<-hs[,c(-1,-2)]
head(hs,6)


##--1.Sampling dataset into train and test

ind<-(sample(nrow(hs),nrow(hs)*0.7))
train_hs<-hs[ind,] 
test_hs<-hs[-ind,] 


##--2.We will use train dataset now onwards
colnames(train_hs)
#1a.Bedrooms (Log tranformation on price gives linear relationship)
boxplot((price)~bedrooms,data=train_hs)
#median of the boxplot of each bedroom shows some relationship between bedroom
#and housing prices

#if we transform price to log(price) we get almost "linear relationship" between
#bedrooms and housing price
plot(train_hs$bedrooms,log(train_hs$price))

boxplot(log(price)~bedrooms,data=train_hs, xlab="Number of Bedrooms",
        ylab="log(Price)",main="Relationship between 
        Number of bedrooms and log(Housing Prices)")
lines(c(0,33),c(12.25,17))



#1b.Bathroom (Log tranformation on price gives linear relationship)
boxplot((price)~bathrooms,data=train_hs)
#median of the boxplot of each bathroom shows some relationship between bathroom
#and housing prices

#if we transform price to log(price) we get almost "linear relationship" between
#bedrooms and housing price
boxplot(log(price)~bathrooms,data=train_hs,xlab="Number of bathrooms",
        ylab="log(Price)",main="Relationship between 
        Number of bathrooms and log(Housing Prices)")
lines(c(0,100),c(12,25))



#1c.sqft_living(shows linear transformation) sqrt transformation on x and log on price
plot(train_hss$sqft_living,train_hss$price)
plot(train_hss$sqft_living,log(train_hss$price))
train_hs$sqft_living_new<-sqrt(train_hs$sqft_living)
plot(train_hs$sqft_living_new,log(train_hs$price),
     xlab="Sqrt(square footage of the home)",
     ylab="log(Price)",main="Relationship between 
     Sqrt(square footage of the home) and log(Housing Prices)")

#1d.sqft_lot(exponential (sqft_lot) +log(price) transformation)
plot(hs$sqft_lot,hs$price)
#it seems some exponential relationship between sqft_lot and price
#asymptote at price=0.5*10^6
train_hs$sqft_lot_new<-10^6*(6.5*(0.2)^(train_hs$sqft_lot/10^4)+0.5)
plot(train_hs$sqft_lot_new,(train_hs$price))
plot(train_hs$sqft_lot_new,log((train_hs$price)),
     xlab="exponent(square footage of the lot)",
     ylab="log(Price)",main="Relationship between 
     exponent(square footage of the lot) and log(Housing Prices)")


#1d.floors (Log tranformation on price gives linear relationship)
boxplot((price)~floors,data=train_hs)
#median of the boxplot of each floors shows some relationship between floors
#and housing prices

#if we transform price to log(price) we get almost "linear relationship" between
#floors and housing price
boxplot(log(price)~floors,data=train_hs,xlab="Floors",
        ylab="log(Price)",main="Relationship between 
        Floors and log(Housing Prices)")
lines(c(0,10),c(12.5,15))

colnames(hs)



#1e.waterfront
#consider 0 as no waterfront view and 1 as waterfront view

boxplot((price)~waterfront,data=train_hs)
#From the graph we can see avg price is high for the houses
#havihng waterfrint view
boxplot(log(price)~waterfront,data=train_hs,
        xlab="House which has a view to a waterfront",
        ylab="log(Price)",main="Relationship between 
        House which has a view to a waterfront and log(Housing Prices)" )

#factor
train_hs$waterfront<-as.factor(train_hs$waterfront)
class(train_hs$waterfront)
colnames(hs)
#1.f.view
boxplot((price)~view,data=train_hs,
        xlab="How many times house been viewed",
        ylab="log(Price)",main="Relationship between 
        How many times house been viewed and log(Housing Prices)")
boxplot(log(price)~view,data=train_hs)
lines(c(0,15),c(12.5,17))
#avg price increases  if it has been viewed more than once


#1.g condition
boxplot((price)~condition,data=train_hs)
boxplot(log(price)~condition,data=train_hs,
        xlab="How good the condition of house is",
        ylab="log(Price)",main="Relationship between 
        How good the condition of house isand log(Housing Prices)")
lines(c(0,25),c(12.5,17))
#avg price increases  with the condition of the house


#1.h.grade
boxplot((price)~grade,data=train_hs)
boxplot(log(price)~grade,data=train_hs,
        xlab="Overall grade given to the housing unit",
        ylab="log(Price)",main="Relationship between 
        Overall grade given to the housing unit and log(Housing Prices)")
lines(c(0,20),c(11.5,17))
#avg price increases  with the grade of the house


#1.i&j.sqft_above and sqft_basement are linear combination of sqft_living
plot(train_hs$sqft_above,train_hs$price)
plot(train_hs$sqft_basement,train_hs$price)
hs_dum<-train_hs
hs_dum$new<-hs_dum$sqft_above+hs_dum$sqft_basement
head(hs_dum$new)
head(hs_dum$sqft_living)
cor(hs_dum$sqft_living,hs_dum$new)

colnames(hs)
#1.k.yr_built
plot(train_hs$yr_built,train_hs$price)



#1.l. yr_renovated modified
train_hs$yr_renovated_new<-ifelse(train_hs$yr_renovated==0,train_hs$yr_built,train_hs$yr_renovated )
plot(hs$yr_renovated,hs$price)

plot(train_hs$yr_renovated_new,log(train_hs$price))



                                                                                                                                                                    #1.m. zipcode #remove
plot(train_hs$zipcode,train_hs$price)



#1.n. lat
plot(train_hss$lat,((train_hss$price)))

plot(train_hs$lat,log((train_hs$price)))



train_hs$lat_new<-poly(train_hs$lat-mean(train_hs$lat),2)

train_hs$lat_new1<-(-37.5)*((train_hs$lat-47.65)^2)+14
plot(train_hs$lat_new1,log(price))



plot(train_hs$lat_new,log((train_hs$price)))

lines(c(47.2,49),c(12,17),col="red")


#1.o long

plot(train_hs$long,((train_hs$price)))

plot(train_hs$long,log((train_hs$price)))
lines(c(-122.4,-121.4),c(13.5,13),col="red")


#1.p sqft_living15
plot(train_hss$sqft_living15,train_hss$price)
plot(train_hss$sqft_living15,log(train_hss$price))
train_hs$sqft_living15_new<-sqrt(train_hs$sqft_living15)
plot(train_hs$sqft_living15_new,log(train_hs$price),
     xlab="sqrt(living area in 2015)",
     ylab="log(Price)",main="Relationship between 
        sqrt(living area in 2015) and log(Housing Prices)")


#1.q sqft_lot15
plot(hs$sqft_lot15,hs$price)
train_hs$sqft_lot15_new<-10^6*(6.5*(0.2)^(train_hs$sqft_lot15/10^4)+0.5)
plot(train_hs$sqft_lot15_new,(train_hs$price))
plot(train_hs$sqft_lot_new,log((train_hs$price)),
     xlab="exponenet(lotSize area in 2015)",
     ylab="log(Price)",main="Relationship between 
        exponent(lotSize area in 2015) and log(Housing Prices)")



##2.models#########################

#2a.--without any transformed variable
#fit<-lm((price)~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+
#          grade+yr_built+yr_renovated+
#          lat+long+sqft_living15+sqft_lot15,data=hs)

#fit<-lm((price)~(bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+
#          view+condition+
#          grade+yr_built+yr_renovated_new+
#          lat_new+long+sqft_living15_new+sqft_lot15_new),data=train_hs)
#
# Used for boxcox transformation 
#fit<-lm((price)~(bedrooms+bathrooms+sqft_living+sqft_lot+floors+
#                   view+condition+waterfront+
#                   grade+yr_built+yr_renovated+
#                   lat+long+sqft_living15+sqft_lot15),data=train_hs)

boxcox(fit)
#fit<-lm(I(log(price))~bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+
#          view+condition+ waterfront+
 #         grade+yr_built+yr_renovated_new+
#          lat_new+long+sqft_living15_new+sqft_lot15_new
 #       +sqft_living_new*waterfront
  #      +grade*waterfront+lat_new*waterfront+long*waterfront+yr_built*waterfront
 #         ,data=train_hs)
#
colnames(train_hs)
summary(fit)
#problem with the summary

#2b. try using transformed model
#fit<-lm(I(log(price))~bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+waterfront+view+condition+
#          grade+yr_built+yr_renovated_new+
#          lat_new+long+sqft_living15_new+sqft_lot15_new,data=train_hs)

#summary(fit)
#qqnorm(residuals(fit))
#plot(fitted.values(fit),rstudent(fit))
#cooks.distance(fit)


library(leaps)
attach(train_hs)
colnames(train_hs)
train_hs$waterfront<-as.factor(train_hs$waterfront)
all <- regsubsets(x=cbind(bedrooms,bathrooms,sqft_living_new,sqft_lot_new,floors,waterfront,view,condition,
                          grade,yr_built,yr_renovated_new,
                          lat_new,long,sqft_living15_new,sqft_lot15_new), y=log(price),  method = "exhaustive", 
                  data=train_hs, all.best = FALSE, nbest = 3)



fit<-lm(log(price)~(bedrooms+bathrooms+sqft_living_new+sqft_lot_new+floors+waterfront+view+condition+
        grade+yr_built+yr_renovated_new+
        lat_new+long+sqft_living15_new+sqft_lot15_new)*waterfront,data=train_hs)
summary(fit)

cor(train_hs$sqft_living,(train_hs$sqft_above+train_hs$sqft_basement))
colnames(train_hs)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(nrow(train_hs)-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)

#exaustive method	
#sqft_living	grade	lat year_built
#sqft_living	grade	 view lat yr_built
#sqft_living	gradeview condition	lat yr_built view


##################3.correlation
#sqft_living_new+view+condition+grade+yr_built+lat_new+sqft_living15_new
colnames(train_hs)
diag(solve(cor(train_hs[,c(-1,-2,-3,-4,-5,-6,-7,-11,-12,-14,-15,-16,-17,-18,-19,-21,-22,-25)])))

cor(train_hs)
colnames(train_hs)
#####.4 residual
getwd()
write.csv(z_low,"z_low")
#Created X matrix 
#X<-as.matrix(cbind(1,train_hs$bedrooms,train_hs$bathrooms,train_hs$sqft_living,
# train_hs$floors,train_hs$waterfront,train_hs$view,
#train_hs$condition,train_hs$grade,train_hs$yr_built,
#train_hs$yr_renovated_new,train_hs$zipcode,train_hs$lat,
#train_hs$long+train_hs$sqft_living15))

X<-as.matrix(cbind(1,train_hs$sqft_living_new,train_hs$view,
                   train_hs$condition,
                   train_hs$grade,train_hs$yr_built,
                   train_hs$lat_new,train_hs$sqft_living15_new)
)
#Created Hat Matrix
H<-X%*%solve((t(X)%*%X))%*%t(X)
0.0057182628
2*(8+1)/15129

hii<-cbind(1:15129,diag(H))
nrow(hii[hii[,2]>0.001189768,])
#Diagonal elements of Hat matrix
diag(H)[diag(H)> 0.001189768]
2*(8)/15129

(cooks.distance(fit_1))[(cooks.distance((fit_1)))>1]
   fitted.values(fit_1)[fitted.values(fit_1)>15.7]#3232
   fitted.values(fit_1)[3232]
exp(15.79807)
predicted =7261304
actual=7700000
lat=47.6298
long=-122.323

predicted =7261304
actual=7700000
lat=47.6298
long=-122.323

a<-train_hs[3232,]

a<-train_hs$price[3232]
a
sum((train_hs$price-exp(fitted.values(fit_1)))^2)/15129
sum((log(train_hs$price)-(fitted.values(fit_1)))^2)/15129
train_hs$price-exp(fitted.values(fit_1))
anova(fit_1)
train_hs[3232,]
min(train_hs$lat)
exp(16)
##Pltting top 3 models
par(mfrow=c(1,1))
fit_1<-lm(log(price)~sqft_living_new+view+condition+grade+yr_built+lat_new+sqft_living15_new,
   data=train_hs)
summary(fit_1)
qqnorm(residuals(fit_1),main="QQ Plot of final model")
plot(fitted.values(fit_1),rstudent(fit_1),main="Residual Plot of final model")


fit_2<-lm(log(price)~sqft_living_new+view+condition+grade+yr_built+lat_new,
          data=train_hs)
summary(fit_2)
qqnorm(residuals(fit_2),main="Model 2")
plot(fitted.values(fit_2),rstudent(fit_2),main="Model 2")

fit_3<-lm(log(price)~sqft_living_new+view+grade+yr_built+lat_new,
          data=train_hs)
summary(fit_3)
qqnorm(residuals(fit_3),main="Model 3")
plot(fitted.values(fit_1),rstudent(fit_3),main="Model 3")



test_hs$sqft_living_new<-sqrt(test_hs$sqft_living)
test_hs$lat_new<-poly(test_hs$lat-mean(test_hs$lat),2)
test_hs$sqft_living15_new<-sqrt(test_hs$sqft_living15)
fit_1<-lm(log(price)~sqft_living_new+view+condition+grade+yr_built+lat_new+sqft_living15_new,
          data=test_hs)
summary(fit_1)
qqnorm(residuals(fit_1),main="Model 1")
plot(fitted.values(fit_1),rstudent(fit_1),main="Model 1")
sum((log(test_hs$price)-(fitted.values(fit_1)))^2)/6484


beta0 <- numeric()
beta1 <- numeric()
beta2 <- numeric()
beta3 <- numeric()
beta4 <- numeric()
beta5 <- numeric()
beta6 <- numeric()
beta7 <- numeric()
beta8 <- numeric()
beta9 <- numeric()
MSP <- numeric()
obs<-c(1:6484)
for (i in 1:1000){
  sample.est <- sort(sample(obs,1000))
  sample.val <- (1:6484)[-sample.est]
  test_hs.est <- test_hs[sample.est,]
  test_hs.val <- test_hs[sample.val,]
  fit_1 <- lm(log(price)~sqft_living_new+view+condition+grade+yr_built+lat_new+sqft_living15_new,
            data=test_hs.est)
  coefficients(fit_1)
  y_hat <- predict(fit_1, test_hs.val[,c(21,9,10,11,14,22,23)])
  pred_error <- test_hs.val[,2] - y_hat
  beta0[i] <- coef(fit_1)[1]
  beta1[i] <- coef(fit_1)[2]
  beta2[i] <- coef(fit_1)[3]
  beta3[i] <- coef(fit_1)[4]
  beta4[i] <- coef(fit_1)[5]
  beta5[i] <- coef(fit_1)[6]
  beta6[i] <- coef(fit_1)[7]
  beta7[i] <- coef(fit_1)[8]
  beta8[i] <- coef(fit_1)[9]
  MSP[i] <- sum(pred_error^2)/1000
}
  
summary(fit_1)
par(mfrow=c(3,3))
hist(beta0,main="Historgram of Intercept",xlab="Intercept")
hist(beta1,main="Historgram of Square footage of the home",xlab="Square footage of the home")
hist(beta2,main="Histogram of view",xlab="View")
hist(beta3,main="Histogram of condition",xlab="Condition")
hist(beta4,main="Histogram of grade",xlab="Grade")
hist(beta5,main="Histrogram of yr_built",xlab="Year built")
hist(beta6,main="Histogram of lat_new1",xlab="lat_new1")
hist(beta7,main="Histrogram of lat_new2",xlab="lat_new2")
hist(beta8,main="Histogram of Square footage of the home in 2015",xlab="Square footage of the home in 2015")
hist(beta9)
colnames(test_hs)
fit_1
hist(MSP)


all <- regsubsets(x=cbind(bedrooms,bathrooms,sqft_living_new,sqft_lot_new,floors,waterfront,view,condition,
                          grade,yr_built,yr_renovated_new,
                          lat_new,long,sqft_living15_new,sqft_lot15_new), y=log(price),  method = "exhaustive", 
                  data=train_hs, all.best = FALSE, nbest = 3)

fit_raw<-lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+
grade+yr_built+yr_renovated+
lat+long+sqft_living15+sqft_lot15,data=train_hs)
summary(fit_raw)

(train_hs$sqft_lot[(hii[hii[,2]>0.0012,])[,1]])
min(train_hs$lat[(hii[hii[,2]>0.0012,])[,1]])
plot((train_hs$lat[(hii[hii[,2]>0.002,])[,1]]),(train_hs$lat[(hii[hii[,2]>0.002,])[,1]]))
plot((train_hs$long[(hii[hii[,2]>0.001189768,])[,1]]),(train_hs$lat[(hii[hii[,2]>0.001189768,])[,1]]),
     xlab="Longitude", ylab="latitude",main="Leverage points")

Apartment$type <- factor(Apartment$type, levels = c(1,2,3), 
                         labels = c("tenement", "blockOfFlats", 
                                    "apartmentBuilding"))
Apartment$FloorType <- factor(Apartment$FloorType, levels = c(0,1,2), 
                              labels = c("low", "middle", 
                                         "high"))
Apartment$ownership <- factor(Apartment$ownership, levels = c(0,1), 
                              labels = c("cooperative", "condominium"))
Apartment$hasBalcony <- factor(Apartment$hasBalcony, levels = c(0,1), 
                               labels = c("no", "yes"))
Apartment$hasElevator<- factor(Apartment$hasElevator, levels = c(0,1), 
                               labels = c("no", "yes"))
Apartment$poiCount<- factor(Apartment$poiCount,levels = c(1,2,3),
                            labels = c("small","medium","large"))
summary(Apartment)
hist(Apartment$price, col = "blue", xlab = "Price, in złoty", freq=F)
xfit <- seq(min(Apartment$price), max(Apartment$price), length=1000)
yfit <- dnorm(xfit, mean=mean(Apartment$price), sd=sd(Apartment$price))
lines(xfit, yfit, col="red", lwd=2)
boxplot(Apartment$price, main= "Boxplot", ylab = "Price")
qqnorm(Apartment$price) 
jarqueberaTest(Apartment$price)
shapiro.test(Apartment$price) 
hist(Apartment$squareMeters, col = "blue", xlab = "size,in square meters", freq=F)
xfit <- seq(min(Apartment$squareMeters), max(Apartment$squareMeters), length=1000)
yfit <- dnorm(xfit, mean=mean(Apartment$squareMeters), sd=sd(Apartment$squareMeters))
lines(xfit, yfit, col="red", lwd=2)
boxplot(Apartment$squareMeters, main= "Boxplot", ylab = "size, in square meters")
qqnorm(Apartment$squareMeters) 
jarqueberaTest(Apartment$squareMeters)
shapiro.test(Apartment$squareMeters)
hist(Apartment$centreDistance, col = "blue", xlab = "distance from ceter in km", freq=F)
xfit <- seq(min(Apartment$centreDistance), max(Apartment$centreDistance), length=1000)
yfit <- dnorm(xfit, mean=mean(Apartment$centreDistance), sd=sd(Apartment$centreDistance))
lines(xfit, yfit, col="red", lwd=2)
boxplot(Apartment$centreDistance, main= "Boxplot", ylab = "distance from ceter in km")
qqnorm(Apartment$centreDistance) 
jarqueberaTest(Apartment$centreDistance)
shapiro.test(Apartment$centreDistance)
cor(Apartment$price, Apartment$squareMeters, method="spearman")
cor.test(Apartment$price, Apartment$squareMeters, method="spearman")
cor(Apartment$price, Apartment$centreDistance, method="spearman")
cor.test(Apartment$price, Apartment$centreDistance, method="spearman")
cor(as.numeric(Apartment$price), 
    as.numeric(Apartment$centreDistance), method="kendall")
cor.test(as.numeric(Apartment$price), 
         as.numeric(Apartment$centreDistance), method="kendall")
table(Apartment$type)
aggregate(Apartment$price, 
          list("type" = Apartment$type), summary)
table(Apartment$FloorType)
aggregate(Apartment$price, 
          list("FloorType" = Apartment$FloorType), summary)
table(Apartment$ownership)
aggregate(Apartment$price, 
          list("ownership" = Apartment$ownership), summary)
table(Apartment$poiCount)
aggregate(Apartment$price, 
          list("poiCount" = Apartment$poiCount), summary)
table(Apartment$hasBalcony)
aggregate(Apartment$price, 
          list("balcony" = Apartment$hasBalcony), summary)
table(Apartment$hasElevator)
aggregate(Apartment$price, 
          list("Elevator" = Apartment$hasElevator), summary)
boxplot(Apartment$price ~ Apartment$hasBalcony, horizontal = T, varwidth = TRUE, 
        main = "Price vs Balcony", xlab = "Balcony")
#correlation analysis

datacorr <-  Apartment[,c("price", "squareMeters","type","FloorType","centreDistance","poiCount","ownership","hasBalcony","hasElevator")]
# Calculating the correlation matrix
cor_matrix <- cor(datacorr)
datacorr <- Apartment[,c("price", "squareMeters","type","FloorType","centreDistance","poiCount","ownership","hasBalcony","hasElevator")]
chart.Correlation(datacorr, histogram=TRUE, pch=19) 
corrplot(cor(datacorr), method = "number", type="full", 
         order = "original", insig = "p-value", sig.level=0.05)
boxplot(Apartment$price ~ Apartment$hasElevator, horizontal = T, varwidth = TRUE, 
        main = "Price vs hasElevator", xlab = "hasElevator")
cor(as.numeric(Apartment$price), 
    as.numeric(Apartment$type), method="kendall")
cor.test(as.numeric(Apartment$price), 
         as.numeric(Apartment$type), method="kendall")
ggplot(data = Apartment, aes(x = squareMeters, y = price)) +
  geom_point(aes(color = "red")) +
  geom_smooth(method = "lm") +
  labs(title = "Scatterplot of squareMeters vs Apartmentprice",
       x = "squareMeters",
       y = "Apartmentprice")
ggplot(data = Apartment, aes(x = centreDistance, y = price)) +
  geom_point(aes(color = "red")) +
  geom_smooth(method = "lm") +
  labs(title = "Scatterplot of centreDistance vs Apartmentprice",
       x = "centreDistance",
       y = "Apartmentprice")
ggplot(data = Apartment, aes(x = centreDistance, y = price)) +
  geom_point(aes(color = "red")) +
  geom_smooth(method = "lm") +
  labs(title = "Scatterplot of centreDistance vs Apartmentprice",
       x = "centreDistance",
       y = "Apartmentprice")
plot(Apartment$price, Apartment$type,
     pch = 20,
     col = "dark green",
     main = "Scatterplot of type vs Apartmentprice",
     xlab = "type",
     ylab = "Apartmentprice")
abline(lm(Apartment$price ~ Apartment$type))
regression1 <- lm(price ~ type + squareMeters + FloorType  + centreDistance + poiCount + ownership  + hasBalcony + hasElevator,data=Apartment)
summary(regression1)
resettest(regression1, power = 2:3, type = c("fitted"))
resettest(regression1, power = 2:3, type = c("regressor"))
regression2 <- lm(log(price) ~    poly(squareMeters,2,raw=T) + poly(centreDistance,3,raw=T)+type+ type*squareMeters +hasBalcony+poiCount + hasElevator + ownership + hasElevator:poiCount + ownership+ ownership*centreDistance,data=Apartment)
summary(regression2)
resettest(regression2, power = 2:3, type = c("fitted"))
resettest(regression2, power = 2:3, type = c("regressor"))
stargazer(regression2, type="text", align=TRUE, style="default", df=FALSE)
plot(regression2, which=2)
hist(regression2$residuals, col = "blue", xlab = "Residuals", freq=F, ylim=c(0,2.5))
lines(density(regression2$residuals), col="red", lwd=2)
par(mfrow=c(1,2))
plot(regression2, which=2)
boxplot(res.std)
par(mfrow=c(1,1))
summary(regression2$residuals)
jarqueberaTest(regression2$residuals)
plot(regression2, which=1)
bptest(regression2, studentize=FALSE)
robust1 = coeftest(regression2, vcov.=vcovHC(regression2, type="HC0"))
show(robust1)
robust2 = coeftest(regression2, vcov.=vcovHC(regression2, type="HC3"))
show(robust2)
stargazer(regression2, robust1, robust2, type="text")
ols_vif_tol(regression2)
regression1 <- lm(price ~ type + squareMeters + FloorType  + centreDistance + poiCount + ownership  + hasBalcony + hasElevator,data=Apartment)
summary(regression1)
ols_regress(price ~ type + squareMeters + FloorType  + centreDistance + poiCount + ownership  + hasBalcony + hasElevator,data=Apartment)
Apartment$lev <- hatvalues(regression1)
Apartment$rstd <- rstandard(regression1)
Apartment$cookd <- cooks.distance(regression1)
lev_threshold <- (2*(length(regression1$coefficients)/nrow(Apartment))) #0.008915
length(Apartment$lev[Apartment$lev > lev_threshold])
length(Apartment$rstd[abs(Apartment$rstd)>2])
cook_threshold <- 4/nrow(Apartment) #0.001486
length(Apartment$cookd[Apartment$cookd > cook_threshold])
nontypical <- Apartment[Apartment$lev > lev_threshold & abs(Apartment$rstd)>2
                      & Apartment$cookd > cook_threshold, ] # 17obs.of 12 variables
plot(regression1, which=4, cook.level= cook_threshold)
abline(h=cook_threshold, lty=2, col= "red")
ols_plot_cooksd_chart(regression1)
plot(regression1, which=5)
influencePlot(regression1, id.method="noteworthy", 
              main="Leverage and residuals", 
              sub= "Circle size is proportional to Cook's distance")
Apartment_new <- Apartment[-c(1012,1640,2290), ] 
regression_new <- lm(price ~ type + squareMeters + FloorType  + centreDistance + poiCount + ownership  + hasBalcony + hasElevator,data=Apartment_new)
summary(regression1)
bptest(regression_new)
jarqueberaTest(regression_new$residuals)
bptest(regression_new)
resettest(regression_new, type="fitted")
resettest(regression_new, type="regressor")
Apartment_new$type <- factor(Apartment_new$type, levels = c(1,2,3), 
                         labels = c("tenement", "blockOfFlats", 
                                    "apartmentBuilding"))
Apartment_new$FloorType <- factor(Apartment_new$FloorType, levels = c(0,1,2), 
                              labels = c("low", "middle", 
                                         "high"))
Apartment_new$ownership <- factor(Apartment_new$ownership, levels = c(0,1), 
                              labels = c("cooperative", "condominium"))
Apartment_new$hasBalcony <- factor(Apartment_new$hasBalcony, levels = c(0,1), 
                               labels = c("no", "yes"))
Apartment_new$hasElevator<- factor(Apartment_new$hasElevator, levels = c(0,1), 
                               labels = c("no", "yes"))
Apartment_new$poiCount<- factor(Apartment_new$poiCount,levels = c(1,2,3),
                            labels = c("small","medium","large"))
regression2new <- lm(log(price) ~    poly(squareMeters,2,raw=T) + poly(centreDistance,3,raw=T)+type+ type*squareMeters +hasBalcony+poiCount + hasElevator + ownership + hasElevator:poiCount + ownership+ ownership*centreDistance,data=Apartment_new)
summary(regression2new)
resettest(regression2new, power = 2:3, type = c("fitted"))
resettest(regression2new, power = 2:3, type = c("regressor"))
jarqueberaTest(regression2new$residuals)
bptest(regression2new)
plot(regression2new, which=1)
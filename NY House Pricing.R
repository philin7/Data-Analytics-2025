#install libraries
install.packages("ggplot2")
install.packages("dplyr")
# Load necessary libraries
library(ggplot2)
library(dplyr)
dataset <- NY_House_Dataset
#Model provided
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]
#MODEL1
lmod <-lm(PRICE ~ BEDS + PROPERTYSQFT, data = dataset)
## print model output
summary(lmod)
ggplot(dataset, aes(x = log10(BEDS)+ log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

#MODEL2
dataset$BATH <- as.numeric(dataset$BATH)
lmod <- lm(PRICE ~ PROPERTYSQFT + BATH, data = dataset)
## print model output
summary(lmod)
ggplot(dataset, aes(x = BATH+ PROPERTYSQFT, y = log10(PRICE))) +
  geom_point()

#MODEL3
lmod <- lm(PRICE ~ BEDS + BATH, data = dataset)
## print model output
summary(lmod)
ggplot(dataset, aes(x = log10(BEDS)+ log10(BATH), y = log10(PRICE))) +
  geom_point()

#Strongest relationship to price for model 2 is PROPERTYSQFT
## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)

lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## fit linear model 2
lmod <- lm(PRICE~BATH, data = dataset)

lmod <- lm(log10(PRICE)~log10(BATH), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~BATH, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(BATH), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## fit linear model 3
lmod <- lm(PRICE~BEDS, data = dataset)

lmod <- lm(log10(PRICE)~log10(BEDS), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~BEDS, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(BEDS), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

library("readxl")

setwd("/Users/gaurav/Documents/Senior Year/Capstone")
df <- read_excel("TestTable1.xlsx")
df


df2 <- df[,-c(2,4,5,6)]
df2

firstlm <- lm(df$`Obesity Rate` ~ df2$`Fast Food 2016` + df2$`Snap Benefits 2017` + df2$`Veg Farms 2012` + df2$`Farmers markets 2018` + df2$`Wic per capita`)
summary(firstlm)

plot(df2$`Obesity Rate`, df2$`Snap Benefits 2017`)
plot(df2$`Obesity Rate`, df2$`Wic per capita`)

plot(df2$`Obesity Rate`, df2$`Farmers markets 2018`)

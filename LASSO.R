library("readxl")
library("corrplot")
setwd <- ("/Users/gaurav/Documents/Senior Year/Capstone")

df <- read_excel("/Users/gaurav/Documents/Senior Year/Capstone/updatedCLEAN_DATA_FOR_ANALYSIS.xlsx")
df1 <- data.frame(df)

 
df2 <- df1
# Replacing the NaNs
for (i in colnames(df1)){
  df2[,i][is.na(df2[,i])] <- median(df2[,i], na.rm = TRUE)
  df2['County'] <- df1['County']
}

df3 <- (df2)

# Standardize the data
for (i in (4:length(df2))){
  df3[,i] <- scale(df3[,i])
}

install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)

# Lasso


colSums(is.na(df3)) 


vec <- colnames(df3)[colSums(is.na(df3)) > 0]
drops <- c(vec)
df3 <- df3[ , !(names(df3) %in% drops)]

x_mat <- as.matrix(df3[,4:length(df3)])

cvfit <- cv.glmnet(x_mat, df3$ObesityRate, family = "poisson")

opt_lam <- cvfit$lambda.min

plot(cvfit)

fit <- glmnet(x_mat, df3$ObesityRate, family = "poisson", lambda = opt_lam)
fit

W <- as.matrix(coef(fit))
W

keep_X <- rownames(W)[W!=0]
keep_X <- keep_X[!keep_X == "(Intercept)"]
X <- x_mat[,keep_X]

mylm <- lm(log(df3$ObesityRate)~X)
summary(mylm)


# IGNORE EVERYTHING BELOW THIS

----------------------------------------------------------------------------------




df1 <- df1[,c(100:177)]
mylm <- lm(df$ObesityRate ~ df$FFR16)

plot(mylm$residuals ~ mylm$fitted.values)
plot(df$ObesityRate ~ df$FFR16)

plot(mylm)

plot(mylm$fitted.values ~ df$FFR16)


plot(log(df$ObesityRate) ~ log(df$`Fast Food`))
abline(mylm)

pairs(df1)

df1[is.na(df1)] <- 0

cor(na.omit(M))
M <- cor(df1)
corrplot(M, method="color")
# data non lin
# transform
# find more data
# Reccomend new sampling procedure 
# Spatial analysis (assumes reklationship in error)
# modeling data that has correlation
# lasso/ridge
# AIC/BIC stepwise
# Not: stepwise, and give results
# Baysean model selection methods --> prior dist
# Next meeting
# better understanding of lasso
# better understanding of PCA
# more research about covariate selection techniques (make a list)
# all the multi colinearity 
# 

install.packages("corrr")
library('corrr')


install.packages("ggcorrplot")
library(ggcorrplot)


install.packages("FactoMineR")
library("FactoMineR")

a <- colSums(is.na(df1))

df1[ , 194:length(df1)][is.na(df1[ , 194:length(df1)])] <- 0
colSums(is.na(df1))

df4 <- df1[, colSums(is.na(df1)) == 0]

df2 <- df4[ ,4:length(df4)-1]

data_normalized <- scale(df2)
df5 <- data_normalized[, colSums(is.na(data_normalized)) == 0]


colSums(is.na(df5))

corr_matrix <- cor(df5)
ggcorrplot(df5)

data.pca <- princomp(df5)
summary(data.pca)

data.pca$loadings
data.pca$loadings[, 1:6]

library("factoextra")
fviz_pca_var(data.pca, col.var = "black")

fviz_eig(data.pca, addlabels = TRUE)
fviz_cos2(data.pca, choice = "var", axes = 1:2)


fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

log_data <- sqrt(df2)
mylm <- lm(log(df$ObesityRate) ~ ., data = log_data)





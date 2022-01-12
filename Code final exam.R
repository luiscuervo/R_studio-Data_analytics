library(rpart) #execute the rpart if I do not use kintmny
library(rpart.plot)
library(caret)

# GERMAN CREDIT

# 1 Read dataset
ds = read.csv("credit-g.csv", sep=",", header = TRUE)
head(ds)
ds$class <- as.factor(ds$class)

# Split dataset
train <- createDataPartition (y=ds$class, p=0.8,list=FALSE)

ds.train <- ds[train,]
ds.test <- ds[-train,]

# Train decision tree:
tree <- rpart(class ~ ., ds.train, method="class")
tree
rpart.plot(tree)

# Test
predictions <- predict(tree, ds.test, type = "class")
confusionMatrix(predictions, ds.test$class)

# KNN
#train
knn.fit <- train(class~ ., data=ds.train, method="knn", preProcess=c("center", "scale"))
knn.fit

#test
knn.predict <- predict(knn.fit, newdata=ds.test)
CM = confusionMatrix (knn.predict, ds.test$class)


# LINEAR REGRESSION

df<-read.csv("insurance.csv", sep=",", stringsAsFactors=TRUE)
df$ClaimNumber = NULL
df$DateReported = NULL
df$DateTimeOfAccident = NULL
df$ClaimDescription = NULL

# Regression algorithm
model = lm(UltimateIncurredClaimCost ~ ., data = df)
summary(model)
# Seran relevantes las variables con baja Pr(>t) - LAS QUE TIENEN ASTERISCOS

# model only with relevant variables:

model2 = lm(UltimateIncurredClaimCost ~ Age + Gender + WeeklyWages + InitialIncurredCalimsCost + MaritalStatus + DependentChildren, data = df)

summary(model2)

#The formula: (cambiar los numeros por los coeficientes)
summary(model2)$coefficients
y = 1.75 + 0.007*Age - 0.23*Gender + 0.001*WeeklyWages + 0.75*CalimsCost - 0.019*MaritalStatusS + 0.2*MaritalStatusU + 0.13*DependentChildren + error  

layout(matrix(1:4,2,2))
plot(model2)

# Invented case
invented.case = data.frame(Age=13, Gender="F", WeeklyWages= 300, InitialIncurredCalimsCost=7, MaritalStatus="M", DependentChildren=1)

# Prediction
predict(model2, invented.case)

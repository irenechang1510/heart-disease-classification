packages <- c("tidyverse", "ggplot2","gridExtra")
sapply(packages, require, character.only=T)

data = read.csv("Heart Disease Dataset.csv")
head(data)
data$target <- as.factor(data$target)
ggplot(data, aes(target, fill=target)) + 
	geom_bar() + 
	theme_classic() + 
	theme(legend.position = "none")

str(data)
summary(data)
any(is.na(data))

cor_data <- data
cor_data$target <- as.numeric(cor_data$target)
cor_matrix <- cor(cor_data)

library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.5, tl.col="black")

ggplot(data, aes(thalach))+
	geom_histogram(aes(y = ..density..),bins=25, fill="red", alpha = 0.5, color = "black")+
	geom_density()+
	theme_classic()+
	facet_wrap(~target)

data$cp <- as.factor(data$cp)
data %>% group_by(cp) %>%
	subset(target == 1) %>%
	ggplot(aes(cp)) + 
	geom_bar(aes(fill = cp)) + 
	theme_classic() + 
	theme(legend.position = "none")

data$exang <- factor(data$exang)
ggplot(data, aes(exang, fill=target))+geom_bar() + theme_classic()

ggplot(data, aes(oldpeak))+
	geom_histogram(aes(y = ..density..),bins=20, fill="green", alpha = 0.5, color = "black")+
	geom_density()+
	theme_classic()+
	facet_wrap(~target)

data$restecg <- factor(data$restecg)
ggplot(data, aes(restecg, fill=restecg)) + 
	geom_bar() + 
	theme_classic()

data$fbs <-factor(data$fbs)
ggplot(data, aes(fbs, fill=fbs)) + 
	geom_bar() + 
	theme_classic()

data$sex <- factor(data$sex)
sex.labs <- c("male", "female")
names(sex.labs) <- c("1", "0")
ggplot(data, aes(age))+
	geom_histogram(bins = 35, fill="blue", alpha = 0.5, color = "black")+
	theme_classic()+
	facet_wrap(sex~target, labeller=labeller(sex=sex.labs))+
	geom_vline(xintercept = mean(data$age), color = "red", linetype = "dashed")

data$slope <- factor(data$slope)
data$ca <- factor(data$ca)
data$thal <- factor(data$thal)
ggplot(data, aes(thal, group = target))+
	geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
	# prop is geom_bar features, not in geom_col
	facet_wrap(~target)+
	theme_classic()

#normalize
data$trestbps <- scale(data$trestbps)
data$chol <- scale(data$chol)
data$thalach <- scale(data$thalach)
data$oldpeak <- scale(data$oldpeak)
summary(data)

### Modeling
#1/ GLM 
library(caTools)
set.seed(4)
sample <- sample.split(data$target,SplitRatio = 0.7)
train <- subset(data, sample == T)
test <- subset(data, sample == F)
glm.fit <- glm(target~., data = train, family=binomial(logit))
#summary(glm.fit)
glm.pred <- predict(glm.fit, newdata=test, type="response")
glm.pred <- ifelse(glm.pred>0.5, 1, 0)
mean(glm.pred != test$target)

#2/ Random forest with k-fold cross validation
library(randomForest)
library(boot)
set.seed(4)
k = 10
folds <-  sample(1:k, nrow(data), replace = T)
cv.errors <- rep(NA,k)
for(j in 1:k){
	rf.model <- randomForest(target~., data = data[folds!= k,])
	rf.pred <- predict(rf.model,data[folds==j,])
	cv.errors[j]= mean((data[folds==j,"target"]!=rf.pred))
}
mean(cv.errors)

#3/ SVM with k-fold cross validation
library(e1071)
set.seed(4)
k = 10
folds <-  sample(1:k, nrow(data), replace = T)
cv.errors <- rep(NA,k)
for(j in 1:k){
	svm.model <- svm(target~., data = data[folds!= k,])
	svm.pred <- predict(svm.model,data[folds==j,])
	cv.errors[j]= mean((data[folds==j,"target"]!=svm.pred))
}
mean(cv.errors)

#4/ Tree methods
library(rpart)
library(rpart.plot)
library(caTools)
set.seed(150)
sample <- sample.split(data$target,SplitRatio = 0.7)
train <- subset(data, sample == T)
test <- subset(data, sample == F)
tree.md <- rpart(target~., method = "class", data = train)
tree.pred <- predict(tree.md, test)
tree.pred <- ifelse(glm.pred>0.5, 1, 0)
mean(tree.pred != test$target)
prp(tree.md)

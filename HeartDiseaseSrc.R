library(dslabs)
library(readr)
library(dplyr)
library(caret)
library(psych)
library(DataExplorer)
library(kableExtra)
library(gridExtra)
library(randomForest)
library(rpart.plot)

att_df <- data.frame(value = c('age','sex','cp','trestbps',
                               'chol','fbs','restecg',' ', ' ', 'thalach',
                               'exang','oldpeak','slope','ca','thal','target'),
                     description = c('age in years','1=male, 0=female',
                                     'chest pain type 1 - typical agina, 2 - atypical agina, 3 - non-anginal pain, 4 - asymptomatic', 
                                     'resting blood pressure in mmHg on admission to the hospital', 
                                     'serum cholesterol in mg/dl', 
                                     'fasting blood sugar > 120 mg/dl, 1 - true, 0 - false', 
                                     'resting electrocardiographic results - 0 - normal, 1 - having ST-T wave abnormality','(T wave inversion and/or ST elevation or depression of > 0.05mv),', 
                                     '2 - showing probable or definite left ventricular hypertrophy by Estes criteria', 'Maximum heart rate achieve', 'exercise induced angina (1 - yes, 0 - no)', 
                                     'ST depression induced by exercise relative to rest','The slope of the peak exercise ST segment', 'Number of major blood vessels(0-3) colored by flourosopy','3-normal,6 - fixed defect, 7 - reversable defect','target 1 or 0'))
att_df %>% kable(caption='Description of Data Attributes') %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left", font_size = 10, latex_options = "hold_position") 

heartDisease <- read_csv("./data/heart.csv")

str(heartDisease)
dim(heartDisease)

head(heartDisease) %>% kable(caption='First 6 Observations from Heart Disease') %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left", font_size = 10, latex_options = "hold_position") 

tail(heartDisease) %>% kable(caption='Last 6 Observations from Heart Disease') %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left", font_size = 10, latex_options = "hold_position") 

sapply(heartDisease, function(x) sum(is.na(x)))

describe(heartDisease) %>% knitr::kable(caption = "Description Statistics for Heart Disease") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left", font_size = 5, latex_options = "hold_position")

par(mfrow = c(5,3), mar = c(2,1,1,1))
hist(heartDisease$age, col="green", main="Age")
hist(heartDisease$sex, col="green", main="Sex")
hist(heartDisease$cp, col="green", main="Chest Pain")
hist(heartDisease$trestbps, col="green", main = "Resting Blood Pressure")
hist(heartDisease$chol, col = "green", main="Cholesterol")
hist(heartDisease$fbs, col="green", main="Fasting Blood Sugar")
hist(heartDisease$restecg, col="green", main="Resting ECG")
hist(heartDisease$thalach, col="green", main="Maxium Heart Rate")
hist(heartDisease$exang, col="green", main="Exercise Induced Agina")
hist(heartDisease$oldpeak, col="green", main="ST Depression")
hist(heartDisease$slope, col="green", main="Slope of ST Segment")
hist(heartDisease$ca, col="green", main="Number of blood Vessels")
hist(heartDisease$thal, col="green", main="Thal")
hist(heartDisease$target, col="green", main="Target")

p1 <- ggplot(heartDisease,aes(x=age))+geom_histogram(binwidth=0.3)+facet_grid(~target)+theme_bw()
p2 <- ggplot(heartDisease,aes(x=sex))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p3 <- ggplot(heartDisease,aes(x=cp))+geom_histogram(binwidth=0.5)+facet_grid(~target)+theme_bw()
p4 <- ggplot(heartDisease,aes(x=trestbps))+geom_histogram(binwidth=0.3) + facet_grid(~target)+theme_bw()

grid.arrange(p1, p2, p3, p4, nrow = 2)

p5 <- ggplot(heartDisease,aes(x=chol))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p6 <- ggplot(heartDisease,aes(x=fbs))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p7 <- ggplot(heartDisease,aes(x=restecg))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p8 <- ggplot(heartDisease,aes(x=thalach))+geom_histogram(binwidth=0.3) + facet_grid(~target)+theme_bw()

grid.arrange(p5,p6,p7,p8, nrow = 2)

p9 <- ggplot(heartDisease,aes(x=exang))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p10 <- ggplot(heartDisease,aes(x=oldpeak))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p11 <- ggplot(heartDisease,aes(x=slope))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p12 <- ggplot(heartDisease,aes(x=ca))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()
p13 <- ggplot(heartDisease,aes(x=thal))+geom_histogram(binwidth=0.5) + facet_grid(~target)+theme_bw()

grid.arrange(p9,p10,p11,p12,p13, nrow =2)

plot_correlation(heartDisease)
par(mfrow = c(2,2), mar = c(2,1,1,1))
plot(heartDisease$cp, heartDisease$target, main="chest pain vs target")
plot(heartDisease$thalach,heartDisease$target, main="thalach vs target")
plot(heartDisease$exang, heartDisease$target, main="exercise induced agina vs target")
plot(heartDisease$oldpeak,heartDisease$target, main="ST depression vs target")

library("PerformanceAnalytics")
chart.Correlation(heartDisease[, c(1:14)], histogram=TRUE, pch=19)

set.seed(1)
y<-heartDisease$target
test_index <- createDataPartition(y,times = 1, p = 0.5, list = FALSE)
train_set <- heartDisease %>% slice(-test_index)
test_set <- heartDisease %>% slice(test_index)

dim(train_set)
dim(test_set)

m<-mean(train_set$target)
m
mean((m - test_set$target)^2)

########## Logistic Model
fit <- glm(factor(target) ~ age + sex + factor(cp) + trestbps + 
             chol + fbs + factor(restecg) + thalach + exang + 
             oldpeak + factor(slope) + factor(ca) + factor(thal), family=binomial(link='logit'), data=train_set)
summary(fit)
fit$coef 
y_hat <- predict(fit, test_set)

y <- ifelse(y_hat > 0.5, 1, 0)
confusionMatrix(data=factor(y), factor(test_set$target))

########## Logistic Model with CV
control <- trainControl(method='cv', number=10, savePredictions=T)

train_glm_cv <- train(factor(target) ~ age + sex + factor(cp) + trestbps + chol + fbs 
                      + factor(restecg) + thalach + exang + oldpeak + factor(slope) 
                      + factor(ca) + factor(thal), data=heartDisease, trControl=control, method='glm', family=binomial(link='logit'))

print(train_glm_cv)
glm_pred <- predict(train_glm_cv, test_set, type = "raw")
cm_lr <- confusionMatrix(data=factor(glm_pred), factor(test_set$target))
cm_lr

varImp(train_glm_cv)

##Knn
knn_fit <- knn3(factor(target) ~ age + sex + factor(cp) + trestbps + 
                  chol + fbs + factor(restecg) + thalach + exang + 
                  oldpeak + factor(slope) + factor(ca) + factor(thal), data = train_set)
y_hat_knn <- predict(knn_fit,test_set, type = "class")
confusionMatrix(data=factor(y_hat_knn), factor(test_set$target))

### Knn with reduced predictors to find maxium k..
train_knn <- train(factor(target) ~ sex + factor(cp) + thalach + exang + 
                     oldpeak, method = "knn",
                   data = train_set, tuneGrid = data.frame(k = seq(1,252)))
ggplot(train_knn, highlight= TRUE)

knn_pred <- predict(train_knn, test_set, type = "raw")
confusionMatrix(data=factor(knn_pred), factor(test_set$target))

### Knn with cross validation
control <-trainControl(method="cv", number = 10, savePredictions=T)
train_knn_cv <- train(factor(target) ~ sex + factor(cp) + thalach + exang + 
                        oldpeak, method = "knn",
                      data = train_set, tuneGrid = data.frame(k = seq(1,252,2)),
                      trControl = control)
ggplot(train_knn_cv, highlight=TRUE)

knn_pred <- predict(train_knn_cv, test_set, type = "raw")
cm_knn <- confusionMatrix(data=factor(knn_pred), factor(test_set$target))
cm_knn

########## Classification Trees
control <- trainControl(method = "cv",
                        number = 10, savePredictions=T)

train_rpart <- train(factor(target) ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = c(0.01)),
                     data = train_set, trControl = control)

#plot(train_rpart)

rpart_pred <- predict(train_rpart, test_set)
cm_ct <- confusionMatrix(data=factor(rpart_pred), factor(test_set$target))
cm_ct
varImp(train_rpart)


rpart.plot(train_rpart$finalModel, type = 4, fallen.leaves = FALSE, box.palette = "GnRd", nn=TRUE)

########### Random Forest
control <- trainControl(method = "cv",
                        number = 10, savePredictions=T)

train_rf <- train(factor(target) ~ .,
                  method = "rf",
                  data = train_set, trControl = control)

plot(train_rf)

pred_rf <- predict(train_rf, test_set)
cm_rf <- confusionMatrix(data = factor(pred_rf),factor(test_set$target))
cm_rf

## display the variable importance
varImp(train_rf)

#### Display results in table format
results_table <- data.frame(methods = c('Regular Guesses', 
                                        'Logistic Regression with K-Fold CV', 
                                        'KNN with K-Fold CV', 
                                        'Classification Tree with K-Fold CV', 
                                        'Random Forest with K-Fold CV'),
                            'Accuracy Values' = c(m,
                                                  cm_lr$overall[["Accuracy"]],
                                                  cm_knn$overall[["Accuracy"]],
                                                  cm_ct$overall[["Accuracy"]],
                                                  cm_rf$overall[["Accuracy"]]),
                            sensitivity = c('--',cm_lr$byClass[1],
                                            cm_knn$byClass[1],
                                            cm_ct$byClass[1],
                                            cm_rf$byClass[1]),
                            specificity = c('--',cm_lr$byClass[2],
                                            cm_knn$byClass[2],
                                            cm_ct$byClass[2],
                                            cm_rf$byClass[2]))
results_table %>% kable(caption="Accuracy Results From Various Methods.") %>%  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 10, latex_options = "hold_position") 


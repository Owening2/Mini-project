3) 
1. iris 데이터에 대해 knn 적용하여 분류 모델 작성(R언어로 해야함)
- iris 데이터를 7:3의 비율로 나눈다(sample함수)
- train 데이터로 knn 모델 생성 (target 컬럼 : species)
- test 데이터로 테스트 수행 

* 주의사항
- 난수 생성시 set.seed(1234)로 설정할 것
- test 데이터 수행시 정확도 출력

set.seed(1234)

iris<-datasets::iris
iris
iris_data <- iris[1:5]
iris_data

idx <- sample(x = c("train", "test"),
              size = nrow(iris_data),
              replace = TRUE,
              prob = c(7,3))

train <- iris_data[idx == "train",]
test <- iris_data[idx == "test",]

train_x <- train[, -5]
test_x <- test[, -5]

train_labels <- train[, 5]
test_labels <- test[, 5]

library(class)

knn_21 <- knn(train = train_x,
             test = test_x,
             cl = train_labels,
             k=21)

knn_21 #knn예측 결과
test_labels #실제 값

plot(formula = Sepal.Length ~ Petal.Width, 
     data = train, 
     col = alpha(c("purple", "blue", "green"), 0.7)[train$Species], main = "KNN (k = 21)")
points(formula = Sepal.Length ~ Petal.Width,
       data = test,
       pch = 17,
       cex = 1.2,
       col = alpha(c("purple", "blue", "green"), 0.7)[knn_21])
       

accuracy_21 <- sum(knn_21 == test_labels) / length(test_labels) 
accuracy_21 #정확도


library(gmodels)

CrossTable(x = test_labels, y=knn_21)

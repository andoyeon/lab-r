# 5. imdb_score와 상관관계가 있는 변수들 회귀트리, 모델트리

library(rpart)

# 모델 학습
# 학습 데이터 세트(75%) / 테스트 데이터 세트(25%)
3807 * 0.75   # 2855
train <- cor_data[1:2855, ]
test <- cor_data[2856:3807, ]

warning(train)

# 학습 데이터 rpart 패키지를 사용해 학습시키기
train_rpart <- rpart(formula = imdb_score ~ ., data = train)
train_rpart
summary(train_rpart)

# 회귀트리 시각화
library(rpart.plot)
rpart.plot(x = train_rpart)

# 모델 평가
movie_predict <- predict(train_rpart, test)

summary(movie_predict)  # 예측
summary(test$imdb_score)  # 실제

# 성능 평가
cor(movie_predict, test$imdb_score)   # 0.398

MAE <- function(actual, predict) {
  return(mean(abs(actual - predict)))
}
MAE(actual = test$imdb_score, predict = movie_predict)  # 1.042

# 모델 성능 향상
library(Cubist)

movie_cubist <- cubist(x = train[-1], y = train$imdb_score)
summary(movie_cubist)

# 모델 트리 성능 테스트
movie_predict2 <- predict(movie_cubist, test)
# 상관 계수
cor(movie_predict2, test$imdb_score)  # 0.42
# 평균 절대 오차
MAE(movie_predict2, test$imdb_score)  # 1.019




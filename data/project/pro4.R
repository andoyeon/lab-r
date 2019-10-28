# 신경망 모델
load("data/project/movie.rda")
load("data/project/cor_data2.rda")

# 데이터확인
str(cor_data2)
summary(cor_data2)

# 정규화
normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

movie_norm <- as.data.frame(lapply(cor_data2, normalization))
summary(movie_norm)

library(neuralnet)

sample_count <- round(nrow(cor_data2)) * 0.75
set.seed(123)
sample_rows <- sample(nrow(cor_data2), sample_count)
# 학습/테스트 데이터 세트
movie_train <- movie_norm[sample_rows, ]
movie_test <- movie_norm[-sample_rows, ]

summary(movie_train$imdb_score)
summary(movie_test$imdb_score)

boxplot(movie_train$imdb_score)
boxplot(movie_test$imdb_score)

# 신경망 모델 생성
set.seed(1234)
movie_model <- neuralnet(formula = imdb_score ~ .,
                         data = movie_train)
# 생성된 NN 확인
plot(movie_model)

# 만들어진 NN 평가
# 테스트 데이터 세트에 적용
model_result <- compute(movie_model, movie_test[-6])
head(model_result)

predict_result <- model_result$net.result
# 상관 계수
cor(predict_result, movie_test$imdb_score) # 0.403

# 모델 향상
movie_model5 <- neuralnet(formula = imdb_score ~ .,
                          data = movie_train,
                          hidden = 5)
plot(movie_model5)

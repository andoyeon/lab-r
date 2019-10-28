cor_data2 <- num_data2[c("duration", "director_facebook_likes", "actor_1_facebook_likes",
                                  "cast_total_facebook_likes", "actor_2_facebook_likes","imdb_score",
                                  "movie_facebook_likes")]
save(cor_data2, file = "data/project/cor_data2.rda")


# 다중 선형 회귀
# 종속 변수 - imdb_score
str(cor_data2)
summary(cor_data2)

# imdb_score 분포도
hist(cor_data2$imdb_score, col = "skyblue", border = "white")

# 상관 행렬
library(psych)
pairs.panels(cor_data2)


#library(corrplot)
#corrplot(as.matrix(cor_data2), method = "ellips", is.corr = F)

# 회귀 모델
lm_movie <- lm(formula = imdb_score ~ ., data = cor_data2)
lm_movie

plot(imdb_score ~ ., data = cor_data2)
return
plot(lm_movie)  # 모델이 적합한지

# 통계적으로 유의한지 여부 확인
summary(lm_movie)   # 모두 유의한 변수 # R-squared: 0.1353

# 회귀 트리
search()
library(rpart)

sample_count <- round(nrow(cor_data2)) * 0.75
set.seed(123)
sample_rows <- sample(nrow(cor_data2), sample_count)

train <- cor_data2[sample_rows, ]
test <- cor_data2[-sample_rows, ]

boxplot(train$imdb_score)
boxplot(test$imdb_score)

# rpart를 이용해 학습시키기
movie_rpart <- rpart(formula = imdb_score ~ ., data = train)
movie_rpart

# 회귀트리 시각화
library(rpart.plot)
rpart.plot(x = movie_rpart, digits = 3, cex = 0.7)
rpart.plot(x = movie_rpart, digits = 3, cex = 0.8, fallen.leaves = T, type = 5)

prp(movie_rpart,box.palette = "auto")

# 모델 평가
movie_predict <- predict(movie_rpart, test)
summary(movie_predict)
summary(test$imdb_score)

# 모델 성능 평가
# 1) 상관 계수
cor(movie_predict, test$imdb_score) # 0.449
# 2) MAE: 평균 절대 오차
MAE <- function(actual, predict) {
  return(mean(abs(actual - predict)))
}
MAE(actual = test$imdb_score, predict = movie_predict)  # 0.761

# 모델 성능 향상
# 모델 트리
library(Cubist)
movie_cubist <- cubist(x = train[-6], y = train$imdb_score)
movie_cubist


# 모델 트리 성능 테스트
movie_predict2 <- predict(movie_cubist, test)
head(movie_predict2)
summary(movie_predict2)
summary(test$imdb_score)

# 모델 성능 평가
# 1) 상관 계수
cor(movie_predict2, test$imdb_score)  # 0.469
# 2) MAE
MAE(actual = test$imdb_score, predict = movie_predict2) # 0.736
# => 성능 좋아짐

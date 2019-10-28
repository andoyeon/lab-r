load("data/project/movie.rda")
search()

boxplot(movie$imdb_score)
hist(movie$imdb_score)

# imdb_score과 관련있는 변수 확인 - 히트맵
num_data <- movie[c("num_critic_for_reviews", "duration", "director_facebook_likes",
                    "actor_3_facebook_likes", "actor_1_facebook_likes", "gross", "num_voted_users",
                    "cast_total_facebook_likes", "facenumber_in_poster", "num_user_for_reviews",
                    "budget", "actor_2_facebook_likes","imdb_score", "aspect_ratio",
                    "movie_facebook_likes")]

summary(num_data)
# install.packages("Hmisc")
library(Hmisc)
c1 <- rcorr(as.matrix(num_data))
View(c1$r)

# install.packages("RColorBrewer")
# install.packages("gplots")
library(RColorBrewer)
library(gplots)
heatmap.2(as.matrix(c1$r), trace = "none", col = brewer.pal(9, "RdBu"),
          cexCol = 0.8, cexRow = 0.8, margins = c(9,9), density.info = "none")


# 히트맵 -> 상관계수
cor(movie$imdb_score, movie$num_voted_users)            # 0.446
cor(movie$imdb_score, movie$num_user_for_reviews)       # 0.318
cor(movie$imdb_score, movie$num_critic_for_reviews)     # 0.316
cor(movie$imdb_score, movie$movie_facebook_likes)       # 0.259
cor(movie$imdb_score, movie$gross)                      # 0.222
cor(movie$imdb_score, movie$duration)                   # 0.274
cor(movie$imdb_score, movie$director_facebook_likes)    # 0.197
cor(movie$imdb_score, movie$cast_total_facebook_likes)  # 0.111


# 상관 행렬
library(psych)
pairs.panels(movie[c("imdb_score", "num_voted_users", "num_user_for_reviews", "num_critic_for_reviews",
                   "movie_facebook_likes", "gross", "duration", "director_facebook_likes",
                   "cast_total_facebook_likes")])


cor_data <- movie[c("imdb_score", "num_voted_users", "num_user_for_reviews", "num_critic_for_reviews",
                    "movie_facebook_likes", "gross", "duration", "director_facebook_likes",
                    "cast_total_facebook_likes")]

# 다중 선형 회귀
movie_model <- lm(formula = imdb_score ~ ., data = cor_data)
movie_model
summary(movie_model)





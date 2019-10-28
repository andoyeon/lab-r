# 1. 데이터 준비
USA_movie <- read.csv("data/project/USA_movie.csv")
movie <- USA_movie

# 2. 데이터 확인, 전처리
str(USA_movie)
summary(USA_movie)

# NA값을 가진 변수
summary(movie[c("num_critic_for_reviews", "duration", "director_facebook_likes",
                "actor_3_facebook_likes", "actor_1_facebook_likes", "gross",
                "facenumber_in_poster", "num_user_for_reviews", "budget",
                "actor_2_facebook_likes", "aspect_ratio")])
sum(is.na(movie[c("num_critic_for_reviews", "duration", "director_facebook_likes",
                   "actor_3_facebook_likes", "actor_1_facebook_likes", "gross",
                   "facenumber_in_poster", "num_user_for_reviews", "budget",
                   "actor_2_facebook_likes", "aspect_ratio")]))
1260 / (3807*14)
# 결측치 1260개 => (1260 / 3807*14) => 0.023
# 결측치 처리(평균 대치)
avg_num_critic_for_reviews <- mean(movie$num_critic_for_reviews, na.rm = T)
avg_num_critic_for_reviews
movie$num_critic_for_reviews <- ifelse(is.na(movie$num_critic_for_reviews),
                                       avg_num_critic_for_reviews, movie$num_critic_for_reviews)

avg_duration <- mean(movie$duration, na.rm = T)
avg_duration
movie$duration <- ifelse(is.na(movie$duration), avg_duration, movie$duration)

avg_director_facebook_likes <- mean(movie$director_facebook_likes, na.rm = T)
movie$director_facebook_likes <- ifelse(is.na(movie$director_facebook_likes),
                                        avg_director_facebook_likes, movie$director_facebook_likes)

avg_actor_3_facebook_likes <- mean(movie$actor_3_facebook_likes, na.rm = T)                                      
movie$actor_3_facebook_likes <- ifelse(is.na(movie$actor_3_facebook_likes),
                                       avg_actor_3_facebook_likes, movie$actor_3_facebook_likes)

avg_actor_1_facebook_likes <- mean(movie$actor_1_facebook_likes, na.rm = T)
movie$actor_1_facebook_likes <- ifelse(is.na(movie$actor_1_facebook_likes),
                                       avg_actor_1_facebook_likes, movie$actor_1_facebook_likes)

avg_gross <- mean(movie$gross, na.rm = T)
movie$gross <- ifelse(is.na(movie$gross), avg_gross, movie$gross)

avg_facenumber_in_poster <- mean(movie$facenumber_in_poster, na.rm = T)
movie$facenumber_in_poster <- ifelse(is.na(movie$facenumber_in_poster),
                                     avg_facenumber_in_poster, movie$facenumber_in_poster)

avg_num_user_for_reviews <- mean(movie$num_user_for_reviews, na.rm = T)
movie$num_user_for_reviews <- ifelse(is.na(movie$num_user_for_reviews), 
                                     avg_num_user_for_reviews, movie$num_user_for_reviews)

avg_budget <- mean(movie$budget, na.rm = T)
movie$budget <- ifelse(is.na(movie$budget), avg_budget, movie$budget)

avg_actor_2_facebook_likes <- mean(movie$actor_2_facebook_likes, na.rm = T)
movie$actor_2_facebook_likes <- ifelse(is.na(movie$actor_2_facebook_likes), 
                                       avg_actor_2_facebook_likes, movie$actor_2_facebook_likes)

avg_aspect_ratio <- mean(movie$aspect_ratio, na.rm = T)
movie$aspect_ratio <- ifelse(is.na(movie$aspect_ratio), avg_aspect_ratio, movie$aspect_ratio)

save(movie, file = "data/project/movie.rda")
load("data/project/movie.rda")

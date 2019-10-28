# imdb_score과 관련있는 변수 확인 - 히트맵
num_data2 <- movie[c("num_critic_for_reviews", "duration", "director_facebook_likes",
                    "actor_3_facebook_likes", "actor_1_facebook_likes", "gross", "num_voted_users",
                    "cast_total_facebook_likes", "facenumber_in_poster", "num_user_for_reviews",
                    "budget", "actor_2_facebook_likes","imdb_score", "movie_facebook_likes")]

str(num_data2)
table(num_data2$budget, useNA = "ifany")

cormat <- round(cor(num_data2), 2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, mapping = aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

lower_tri <- get_lower_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(lower_tri, na.rm = T)
# heatmap
ggplot(data = melted_cormat, mapping = aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Movie Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1)) +
  coord_fixed()
  
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkred", high = "yellow", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Movie\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


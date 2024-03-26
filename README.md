# Sample music data
music_data <- data.frame(
  user_id = c(1, 1, 1, 2, 2, 3, 3, 3),
  song_id = c(101, 102, 103, 101, 104, 102, 103, 105),
  rating = c(5, 4, 3, 4, 5, 3, 2, 1)
)

# Create a user-song matrix
music_matrix <- reshape2::dcast(music_data, user_id ~ song_id, value.var = "rating", fill = 0)

# Calculate similarity between users based on ratings
user_similarity <- as.matrix(proxy::simil(music_matrix, method = "cosine"))

# Function to recommend music for a given user
recommend_music <- function(user_id, n_recommendations = 3) {
  user_ratings <- music_matrix[user_id, ]
  similar_users <- user_similarity[user_id, ]
  
  # Calculate weighted average of ratings from similar users
  weighted_avg_ratings <- colSums(music_matrix[,-1] * similar_users) / sum(similar_users)
  
  # Exclude songs already rated by the user
  unrated_songs <- which(user_ratings == 0)
  
  # Sort unrated songs by predicted rating
  recommended_songs <- sort(weighted_avg_ratings[unrated_songs], decreasing = TRUE)
  
  # Get top n recommendations
  top_recommendations <- names(recommended_songs)[1:n_recommendations]
  
  return(top_recommendations)
}

# Example: Recommend music for user 2
recommendations <- recommend_music(2, n_recommendations = 3)
print(recommendations)

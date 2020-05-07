library(recommenderlab) 
data(MovieLense) 
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50, 
                             colCounts(MovieLense) > 100] 

# Splitting the data into train and test sets
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies), 
                      replace = TRUE, prob = c(0.8, 0.2)) 
recc_data_train <- ratings_movies[which_train, ] 
recc_data_test <- ratings_movies[!which_train, ]

# Item-based collaborative filtering (IBCF)
#....
ibcf_recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30)) 
ibcf_model_details <- getModel(ibcf_recc_model) 
ibcf_model_details

# Identify the top n recommended movies to the users in the test set

n_recommended <- 6 #the number of items to recommend to each user
ibcf_recc_predicted <- predict(object = ibcf_recc_model, newdata = recc_data_test, n = n_recommended) 
ibcf_recc_predicted 

# define a matrix to give the recommendations for each user
ibcf_recc_matrix <- sapply(ibcf_recc_predicted@items, function(x){ colnames(ratings_movies)[x] 
}) 
View(ibcf_recc_matrix[, 1:3]) 

# User-based collaborative filtering (UBCF)
# Building UBCF model
ubcf_recc_model <- Recommender(data = recc_data_train, method = "UBCF") 
ubcf_model_details <- getModel(ubcf_recc_model) 
ubcf_model_details 

# Determine the top five recommendations for each new user
n_recommended <- 6 
ubcf_recc_predicted <- predict(object = ubcf_recc_model, newdata = recc_data_test, n = n_recommended) 
ubcf_recc_predicted # Recommendations as 'topNList' with n = 5 for 104 users.

ubcf_recc_matrix <- sapply(ubcf_recc_predicted@items, function(x){ colnames(ratings_movies)[x] 
}) 
View(ubcf_recc_matrix[, 1:4])

######################################################

recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix") 
names(recommender_models)

image(MovieLense, main = "Heatmap of the rating matrix")

min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)

image(MovieLense[rowCounts(MovieLense) > min_n_movies,colCounts(MovieLense) > min_n_users], main ="Heatmap of the top users and movies")

# visualize the top 2 percent of users and movies in the new rating matrix

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,colCounts(MovieLense) > 100]
min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)

image(ratings_movies[rowCounts(ratings_movies) > min_movies,colCounts(ratings_movies) > min_users], main = "Heatmap of the top users and movies")


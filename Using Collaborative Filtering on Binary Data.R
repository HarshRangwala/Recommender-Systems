library(recommenderlab) 
data(MovieLense) 
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50, 
                             colCounts(MovieLense) > 100] 

ratings_movies_viewed <- binarize(ratings_movies,minRating = 1) 

which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies_viewed),replace = TRUE, prob = c(0.8, 0.2)) 
recc_data_train <- ratings_movies_viewed [which_train, ] 
recc_data_test <- ratings_movies_viewed [!which_train, ] 

recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(method = "Jaccard")) 
model_details <- getModel(recc_model)

n_recommended <- 6 
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended) 
recc_matrix <- sapply(recc_predicted@items, function(x){ 
  colnames(ratings_movies)[x] 
}) 
View(recc_matrix[, 1:4]) 
library(proxy)
library(recommenderlab)
library(reshape2)
movies <- read.csv("movies.csv", header = TRUE, stringsAsFactors = FALSE)
ratings <- read.csv("ratings.csv", header = TRUE)
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE), ]

movie_recommendation <- function(input, input2, input3) {
  row_num <- which(movies2[, 2] == input)
  row_num2 <- which(movies2[, 2] == input2)
  row_num3 <- which(movies2[, 2] == input3)
  userSelect <- matrix(NA, 10325)
  userSelect[row_num] <- 5 # хардкод першої вибірки до рейтингу 5
  userSelect[row_num2] <- 4 # хардкод другої вибірки до рейтингу 4
  userSelect[row_num3] <- 3 # хардкод третьої вибірки до рейтингу 3
  userSelect <- t(userSelect)
  
  ratingmat <-
    dcast(ratings,
          userId ~ movieId,
          value.var = "rating",
          na.rm = FALSE)
  ratingmat <- ratingmat[, -1]
  colnames(userSelect) <- colnames(ratingmat)
  ratingmat2 <- rbind(userSelect, ratingmat)
  ratingmat2 <- as.matrix(ratingmat2)
  
  # Конвертувати рейтингову матрицю у розріджену матрицю:
  ratingmat2 <- as(ratingmat2, "realRatingMatrix")
  
  # Створити Рекомендаційну систему. "UBCF" означає Колаборативну фільтрацію (user-based collaborative filtering)
  recommender_model <-
    Recommender(ratingmat2,
                method = "UBCF",
                param = list(method = "Cosine", nn = 30))
  recom <- predict(recommender_model, ratingmat2[1], n = 10)
  recom_list <- as(recom, "list")
  no_result <- data.frame(matrix(NA, 1))
  recom_result <- data.frame(matrix(NA, 10))
  if (as.character(recom_list[1]) == 'character(0)') {
    no_result[1, 1] <-
      "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
    colnames(no_result) <- "No results"
    return(no_result)
  } else {
    for (i in c(1:10)) {
      recom_result[i, 1] <- as.character(subset(movies,
                                                movies$movieId == as.integer(recom_list[[1]][i]))$title)
    }
    colnames(recom_result) <- " Titles:  "
    return(recom_result)
  }
}
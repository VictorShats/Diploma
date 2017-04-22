movies <- read.csv("movies.csv", stringsAsFactors = FALSE)
ratings <- read.csv("ratings.csv")

library(recommenderlab)
library(ggplot2)

# Провести попередю обробку даних:
genres <- as.data.frame(movies$genres, stringsAsFactors = FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[, 1], '[|]',
                                   type.convert = TRUE),
                         stringsAsFactors = FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # ми маємо 18 жанри в цілому

genre_matrix <-
  matrix(0, 10330, 18) # порожня матриця, 10330=кількість фільмів+1, 18=кількість жанрів
genre_matrix[1, ] <- genre_list # встановити першим рядком список жанрів
colnames(genre_matrix) <- genre_list # встановити імена стовпців для списку жанрів

# Зробити перебір по матриці:
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1, ] == genres2[i, c])
    genre_matrix[i + 1, genmat_col] <- 1
  }
}

# Конвертувати в dataframe:
genre_matrix2 <-
  as.data.frame(genre_matrix[-1, ], stringsAsFactors = FALSE) # видалити перший рядок - список жанрів
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[, c] <- as.integer(genre_matrix2[, c]) # конвертувати символи в цілі числа
}

# Створити матрицю для пошуку фільму за жанром:
years <- as.data.frame(movies$title, stringsAsFactors = FALSE)
library(data.table)
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
years <-
  as.data.frame(substr(substrRight(substrRight(
    years$`movies$title`, 6
  ), 5), 1, 4))

search_matrix <-
  cbind(movies[, 1], substr(movies[, 2], 1, nchar(movies[, 2]) - 6), years, genre_matrix2)
colnames(search_matrix) <- c("movieId", "title", "year", genre_list)

write.csv(search_matrix, "search.csv")
search_matrix <- read.csv("search.csv", stringsAsFactors = FALSE)

# Приклад пошуку бойовиків, знятих в 2003 році:
subset(search_matrix, Action == 1 & year == 2003)$title

## Створити профіль користувача
binaryratings <- ratings

# рейтинги 4 і 5 помічаються як 1,
# та представляють фільми, які можуть сподобатися;
# рейтинги 3 і нижче, помічаються як -1, та представляють антипатії:

for (i in 1:nrow(binaryratings)) {
  if (binaryratings[i, 3] > 3) {
    binaryratings[i, 3] <- 1
  }
  else{
    binaryratings[i, 3] <- -1
  }
}

# Перетворити матрицю двійкових рейтингів в правильний формат:
binaryratings2 <-
  dcast(binaryratings,
        movieId ~ userId,
        value.var = "rating",
        na.rm = FALSE)

for (i in 1:ncol(binaryratings2)) {
  binaryratings2[which(is.na(binaryratings2[, i]) == TRUE), i] <- 0
}
binaryratings2 = binaryratings2[, -1] # видалити колонку movieIds, рядки movieIds замінити на рядки userIds


# Видалити рядки, що не мають рейтингу фільмів з набору даних :
movieIds <- length(unique(movies$movieId)) # 10329
ratingmovieIds <- length(unique(ratings$movieId)) # 10325
movies2 <-
  movies[-which((movies$movieId %in% ratings$movieId) == FALSE), ]
rownames(movies2) <- NULL

# Видалити рядки, що не мають рейтингу фільмів з genre_matrix2 :
genre_matrix3 <-
  genre_matrix2[-which((movies$movieId %in% ratings$movieId) == FALSE), ]
rownames(genre_matrix3) <- NULL

# Обчислити скалярний добуток матриці жанрів і матриці рейтингів
# і отримати профілі користувачів.

# Обчислити скалярний добуток для профілів користувачів:
result = matrix(0, 18, 668) # тут, 668 = кількості користувачів/оцінок, 18 = кількості жанрів
for (c in 1:ncol(binaryratings2)) {
  for (i in 1:ncol(genre_matrix3)) {
    result[i, c] <-
      sum((genre_matrix3[, i]) * (binaryratings2[, c])) # оцінки кожного жанру
  }
}

# Перетворити в двійковий формат:
for (c in 1:ncol(result)) {
  for (i in 1:nrow(result)) {
    if (result[i, c] < 0) {
      result[i, c] <- 0
    }
    else {
      result[i, c] <- 1
    }
  }
}

# Припустимо, що користувачам подобаються аналогічні речі, і вони отримуватимуть фільми,
# які найближче підходять за подібністю з профілями цих користувачів.
# використаю Коефіцієнт Жаккара для вимірювання подібності між профілями користувачів.

# Колаборативна фільтрація (UBCF - User-Based Collaborative Filtering) :

library(reshape2)
# Створити матрицю рейтингів. Рядки = userId, Стовпці = movieId :
ratingmat <-
  dcast(ratings,
        userId ~ movieId,
        value.var = "rating",
        na.rm = FALSE)
ratingmat <- as.matrix(ratingmat[, -1]) # видалити userIds

# Метод: UBCF - Колаборативна фільтрація
# Метод обрахунку міри подібності: Косинусний коефіцієнт (Cosine Similarity)
# Найближчих сусідів (Nearest Neighbors): 30

library(recommenderlab)
# Перетворити матрицю рейтингів в розріджену матрицю бібліотеки recommenderlab :
ratingmat <- as(ratingmat, "realRatingMatrix")

# Визначити, наскільки схожі перші чотири користувачі один з одним.
# Створити матрицю подібності:
similarity_users <- similarity(ratingmat[1:4,],
                               method = "cosine",
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "Схожість користувачів")

# Обчислити схожість перших чотирьох фільмів:
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Схожість фільмів")

# Дослідити вагомість оцінок:
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # які значення оцінок - унікальні

table_ratings <-
  table(vector_ratings) # кожне номінальне значення
table_ratings

# Візуалізувати рейтинг:
vector_ratings <-
  vector_ratings[vector_ratings != 0] # рейтинг == 0 це не сумісні значення
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) +
  ggtitle("Розподіл рейтингів")

# Дослідити перегляди фільмів:
views_per_movie <- colCounts(ratingmat) # число переглядів для кожного фільму

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # створити dataframe переглядів
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE),] # сортувати за кількістю переглядів

ggplot(table_views[1:6,], aes(x = movie, y = views)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = subset(movies2, movies2$movieId == table_views$movie)$title) +
  ggtitle("Кількість переглядів найкращих фільмів")

# Візуалізувати матрицю:
image(ratingmat, main = "Теплокарта (heatmap) матриці рейтингів") # важко прочитати занадто багато вимірів
image(ratingmat[1:10, 1:15], main = "Теплокарта (heatmap) перших 10 рядків і 15 стовпців")
image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)],
      main = "Теплокарта (heatmap) найкращих користувачів та найкращих фільмів")


# Нормалізувати дані:
ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                     colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)],
      main = "Теплокарта (heatmap) найкращих користувачів та найкращих фільмів")

# Створити модель Колоборативної фільтрації (UBCF):
recommender_model <- Recommender(ratingmat_norm,
                                 method = "UBCF",
                                 param = list(method = "Cosine", nn = 30))

model_details <- getModel(recommender_model)
model_details$data

recom <- predict(recommender_model,
                 ratingmat[1],
                 n = 10) # Отримати 10 найкращих рекомендацій для 1-го користувача з набору даних

recom

recom_list <- as(recom,
                 "list") #перетворити об'єкт бібліотеки recommenderlab в читабельний список

# Отримати рекомендації:
recom_result <- matrix(0, 10)
for (i in 1:10) {
  recom_result[i] <- as.character(subset(movies,
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}


# Оцінка:
evaluation_scheme <- evaluationScheme(
  ratingmat,
  method = "cross-validation",
  k = 5,
  given = 3,
  goodRating = 5
) # k=5 означає 5-кратну поперечну перевірку. given=3 означає Given-3 протокол

evaluation_results <- evaluate(evaluation_scheme,
                               method = "UBCF",
                               n = c(1, 3, 5, 10, 15, 20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]

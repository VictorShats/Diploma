library(shiny)
library(markdown)
library(ggvis)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "shortcut icon", href = "favicon.ico")
  ),
  
  
  h1("VikToFilm — Рекомендаційна система фільмів", align = "center"),
  
  
  p(
    tags$a(
      href = "http://www.facebook.com/sharer.php?u=https://viktorshatskykh.shinyapps.io/diploma/",
      target = "_blank",
      tags$img(height = "20px",
               src = "fb2.png")
    ),
    a(
      href = "http://www.linkedin.com/shareArticle?mini=true&url=https://viktorshatskykh.shinyapps.io/diploma/",
      target = "_blank",
      tags$img(height = "20px",
               src = "linkedin.png")
    ),
    a(
      href = "http://twitter.com/share?url=https://viktorshatskykh.shinyapps.io/diploma/",
      target = "_blank",
      tags$img(height = "20px",
               src = "twitter.png")
    ),
    a(
      href = "https://github.com/VictorShats",
      target = "_blank",
      tags$img(height = "20px",
               src = "github.png")
    ),
    align = "center"
  ),
  
  navbarPage(
    "VikToFilm",
    tabPanel(
      "Рекомендаційна система фільмів",
      fluidRow(
        h2(
          "Новий спосіб шукати улюблені фільми — довірте це штучному інтелекту!",
          align = "center"
        ),
        
        
        column(
          4,
          p(img(
            src = "step1.png",
            height = 200,
            width = 200
          ), align = "center"),
          h3(
            "Виберіть жанри фільмів, яким Ви надаєте перевагу (сортуйте за пріоритетом):",
            align = "center"
          ),
          br(),
          wellPanel(
            selectInput("input_genre", "Жанр №1",
                        genre_list),
            selectInput("input_genre2", "Жанр №2",
                        genre_list),
            selectInput("input_genre3", "Жанр №3",
                        genre_list)
            #submitButton("Оновити Список Фільмів")
          )
        ),
        
        column(
          4,
          p(img(
            src = "step2.png",
            height = 200,
            width = 200
          ), align = "center"),
          h3(
            "Виберіть фільми, які Ви вже переглянули, та які Вам подобаються в кожному з цих жанрів:",
            align = "center"
          ),
          br(),
          wellPanel(# Ці виводи для динамічного UI
            uiOutput("ui"),
            uiOutput("ui2"),
            uiOutput("ui3")
            #submitButton("Отримати Рекомендації")
          )),
        
        column(
          4,
          p(img(
            src = "step3.jpg",
            height = 200,
            width = 200
          ), align = "center"),
          h3(
            "Отримайте рекомендації! Вам повинні сподобатися ще такі фільми:",
            align = "center"
          ),
          br(),
          tableOutput("table")
          #verbatimTextOutput("dynamic_value")
        )
      ),
      
      fluidRow(column(
        12,
        h5(
          "Дипломна роботи на тему: Інформаційна система релевантності фільмів до потреб користувача",
          align = "center"
        ),
        h5(
          "© 2017 | Всі права захищені | ",
          a("Віктор Шатських", href = "https://www.facebook.com/victorshats", target =
              "_blank"),
          align = "center"
        )
        
      ))
    ),
    tabPanel("Про алгоритм",
             fluidRow(column(
               12,
               h5(
                 "Дипломна роботи на тему: Інформаційна система релевантності фільмів до потреб користувача",
                 align = "center"
               ),
               h5(
                 "© 2017 | Всі права захищені | ",
                 a("Віктор Шатських", href = "https://www.facebook.com/victorshats", target =
                     "_blank"),
                 align = "center"
               )
               
             ))),
    navbarMenu("Більше",
               tabPanel("Про проект"),
               tabPanel("Контакти"))
  )
)
)
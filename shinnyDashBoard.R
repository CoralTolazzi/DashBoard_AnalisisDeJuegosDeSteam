library(shiny)
library(tm)
library(wordcloud)
library(stringr)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

# 1) Cargar datos
data <- read.csv("bestSelling_games.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# 2) Preprocesar datos
prepared_data <- data %>%
  mutate(
    reviews_like_rate = as.numeric(str_replace(reviews_like_rate, "%", "")),
    all_reviews_number = as.numeric(all_reviews_number),
    estimated_downloads = as.numeric(estimated_downloads),
    rating = as.numeric(rating),
    price = as.numeric(price),
    release_year = as.numeric(substr(release_date, 1, 4)),
    difficulty = factor(difficulty, levels = 1:5, 
                        labels = c("Muy fácil", "Fácil", "Media", "Difícil", "Muy difícil")),
    length = factor(case_when(
      length <= 10 ~ "Corta (≤10h)",
      length <= 20 ~ "Media (11-20h)",
      length <= 40 ~ "Larga (21-40h)",
      TRUE ~ "Muy larga (>40h)"
    ), levels = c("Corta (≤10h)", "Media (11-20h)", "Larga (21-40h)", "Muy larga (>40h)")),
    age_restriction = factor(case_when(
      age_restriction == 0 ~ "Para todos",
      age_restriction <= 10 ~ "10+",
      age_restriction <= 13 ~ "13+",
      age_restriction <= 17 ~ "17+",
      TRUE ~ "Desconocida"
    ), levels = c("Para todos", "10+", "13+", "17+", "Desconocida"))
  ) %>%
  filter(!is.na(reviews_like_rate), !is.na(all_reviews_number))

# Preprocess tags
process_tags <- function(tags) {
  tags %>%
    tolower() %>%
    str_split(",") %>%
    unlist() %>%
    trimws() %>%
    str_replace_all("[^[:alnum:]]", "") %>%
    .[. != ""] %>%
    .[!grepl("^[0-9]+$", .)]
}

all_tags <- process_tags(prepared_data$user_defined_tags)
tags_freq <- table(all_tags) %>% sort(decreasing = TRUE)

# Preprocess OS
process_os <- function(os) {
  os_data <- os %>%
    tolower() %>%
    str_split(",") %>%
    lapply(trimws) %>%
    lapply(function(x) {
      case_when(
        all(c("win", "mac", "linux") %in% x) ~ "Windows + Mac + Linux",
        all(c("win", "mac") %in% x) ~ "Windows + Mac",
        all(c("win", "linux") %in% x) ~ "Windows + Linux",
        "win" %in% x ~ "Windows solo",
        "mac" %in% x ~ "Mac solo",
        "linux" %in% x ~ "Linux solo",
        TRUE ~ "Otros"
      )
    }) %>%
    unlist()
  
  table(os_data) %>%
    as.data.frame() %>%
    rename(OS = os_data, Count = Freq) %>%
    filter(OS != "Otros") %>%
    arrange(desc(Count))
}

os_data <- process_os(prepared_data$supported_os)

# 3) UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        font-family: Arial, sans-serif;
      }
      .well {
        background-color: #ffffff;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .tab-content {
        background-color: #ffffff;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      h3 {
        color: #333333;
        border-bottom: 1px solid #eeeeee;
        padding-bottom: 10px;
      }
    "))
  ),
  
  titlePanel(
    div(
      "Análisis de juegos de Steam 2024",
      h4("Análisis de 2380 Juegos", style = "color: #666666;")
    )
  ),
  
  div(
    style = "text-align: center; margin-bottom: 20px;",
    img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Steam_icon_logo.svg/512px-Steam_icon_logo.svg.png", 
        height = "100px")
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("maxWords",
                  "Cantidad máxima de palabras:",
                  min = 10, max = 50, value = 30),
      radioButtons("rating_type", 
                   "Mostrar compañías con:",
                   choices = c("Mejor rating" = "best",
                               "Peor rating" = "worst"),
                   selected = "best"),
      sliderInput("min_games",
                  "Mínimo juegos por compañía:",
                  min = 1, max = 10, value = 3),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Análisis de especifico de cada Juego",
                 h3("Géneros y Estilos más comunes"),
                 plotOutput("wordcloud"),
                 br(), hr(),
                 h3("Top 15 Juegos Más Descargados"),
                 plotOutput("top15_downloads_plot"),
                 br(), hr(),
                 h3("Relación Precio-Descargas"),
                 plotlyOutput("price_downloads_scatter"),
                 h4("Juegos seleccionados:"),
                 DTOutput("games_table"),
                 DTOutput("selected_games"),
                 br(), hr(),
                 h3("Top 15 Juegos con Mejor Rating"),
                 plotOutput("top15_rating")
        ),
        tabPanel("Análisis de Compañías",
                 h3(textOutput("companies_title")),
                 plotOutput("companies_plot"),
                 br(), hr(),
                 h3("Detalles por Compañía"),
                 DTOutput("company_details")
        ),
        tabPanel("Comparacion de cada Juego",
                 h3("Comparación por Dificultad"),
                 plotOutput("difficulty_pie"),
                 br(), hr(),
                 h3("Comparación por Duración"),
                 plotOutput("length_pie"),
                 br(), hr(),
                 h3("Comparación por Edad"),
                 plotOutput("age_pie"),
                 br(), hr(),
                 h3("Comparación por Sistemas Operativos"),
                 plotOutput("os_pie")
        )
      ),
      width = 9
    )
  )
)

# 4) Server
server <- function(input, output, session) {
  
  output$wordcloud <- renderPlot({
    filtered_tags <- tags_freq[tags_freq >= 3]
    
    if(length(filtered_tags) == 0) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No hay suficientes tags", cex = 1.2)
    } else {
      wordcloud(
        names(filtered_tags), filtered_tags,
        max.words = input$maxWords,
        colors = brewer.pal(8, "Dark2"),
        scale = c(3, 0.5)
      )
    }
  })
  
  output$top15_downloads_plot <- renderPlot({
    top15 <- prepared_data %>%
      select(game_name, estimated_downloads) %>%
      arrange(desc(estimated_downloads)) %>%
      head(15) %>%
      mutate(
        estimated_downloads_millions = estimated_downloads / 1e6,
        game_name = factor(game_name, levels = game_name[order(estimated_downloads_millions, decreasing = FALSE)])
      )
    
    ggplot(top15, aes(x = game_name, y = estimated_downloads_millions)) +
      geom_col(fill = "#1a9fff") +
      labs(x = NULL, y = "Descargas (millones)", title = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label = round(estimated_downloads_millions, 1)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  output$top15_rating <- renderPlot({
    top15_rating <- prepared_data %>%
      select(game_name, rating, all_reviews_number) %>%
      arrange(desc(rating)) %>%
      head(15) %>%
      mutate(game_name = factor(game_name, levels = game_name[order(rating, decreasing = FALSE)]))
    
    ggplot(top15_rating, aes(x = game_name, y = rating)) +
      geom_col(fill = "#ff9900") +
      labs(x = NULL, y = "Rating (1-5)", title = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label = round(rating, 2)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 5), expand = expansion(mult = c(0, 0.1)))
  })
  
  scatter_data <- reactive({
    prepared_data %>%
      filter(!is.na(price), !is.na(estimated_downloads)) %>%
      mutate(
        estimated_downloads_millions = estimated_downloads/1e6,
        hover_text = paste(
          "Juego:", game_name, "<br>",
          "Desarrollador:", developer, "<br>",
          "Precio: $", round(price, 2), "<br>",
          "Descargas:", round(estimated_downloads_millions, 1), "millones<br>",
          "Rating:", round(rating, 2), "<br>",
          "Dificultad:", difficulty, "<br>",
          "Edad:", age_restriction
        )
      )
  })
  
  output$price_downloads_scatter <- renderPlotly({
    plot_data <- scatter_data()
    
    selected_rows <- input$games_table_rows_selected
    if(!is.null(selected_rows)) {
      selected_games <- plot_data[selected_rows, "game_name"]
      plot_data <- plot_data %>%
        mutate(selected = game_name %in% selected_games)
    } else {
      plot_data$selected <- FALSE
    }
    
    plot_data <- plot_data %>%
      mutate(
        point_color = ifelse(selected, "#ff4d4d", "#1a9fff"),
        point_size = ifelse(selected, 6, 4)
      )
    
    p <- ggplot(plot_data, aes(x = price, y = estimated_downloads_millions, text = hover_text)) +
      geom_point(aes(color = point_color, size = point_size), alpha = 0.7) +
      scale_color_identity() +
      scale_size_identity() +
      labs(x = "Precio (USD)", y = "Descargas (millones)", title = NULL) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(limits = c(0, max(plot_data$price, na.rm = TRUE) * 1.1)) +
      scale_y_continuous(limits = c(0, max(plot_data$estimated_downloads_millions, na.rm = TRUE) * 1.1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(dragmode = "select")
  })
  
  output$games_table <- renderDT({
    datatable(
      scatter_data() %>%
        select(game_name, developer, price, estimated_downloads_millions, rating) %>%
        mutate(
          price = round(price, 2),
          estimated_downloads_millions = round(estimated_downloads_millions, 1),
          rating = round(rating, 2)
        ) %>%
        rename(
          "Juego" = game_name,
          "Desarrollador" = developer,
          "Precio (USD)" = price,
          "Descargas (millones)" = estimated_downloads_millions,
          "Rating" = rating
        ),
      selection = list(mode = 'multiple', selected = NULL),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$selected_games <- renderDT({
    selected_rows <- input$games_table_rows_selected
    if(is.null(selected_rows)) return(NULL)
    
    selected_data <- scatter_data() %>%
      slice(selected_rows) %>%
      select(game_name, developer, price, estimated_downloads_millions, rating, difficulty, age_restriction) %>%
      mutate(
        price = round(price, 2),
        estimated_downloads_millions = round(estimated_downloads_millions, 1),
        rating = round(rating, 2)
      ) %>%
      rename(
        "Juego" = game_name,
        "Desarrollador" = developer,
        "Precio (USD)" = price,
        "Descargas (millones)" = estimated_downloads_millions,
        "Rating" = rating,
        "Dificultad" = difficulty,
        "Edad Recomendada" = age_restriction
      )
    
    datatable(selected_data, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$companies_title <- renderText({
    if (input$rating_type == "best") {
      paste("Top Compañías con Mejor Rating (mínimo", input$min_games, "juegos)")
    } else {
      paste("Top Compañías con Peor Rating (mínimo", input$min_games, "juegos)")
    }
  })
  
  companies_data <- reactive({
    req(input$min_games)
    
    prepared_data %>%
      group_by(developer) %>%
      summarise(
        avg_rating = mean(rating, na.rm = TRUE),
        n_games = n(),
        total_downloads = sum(estimated_downloads, na.rm = TRUE)
      ) %>%
      filter(n_games >= input$min_games) %>%
      {
        if (input$rating_type == "best") {
          arrange(., desc(avg_rating)) %>% head(15)
        } else {
          arrange(., avg_rating) %>% head(15)
        }
      }
  })
  
  output$companies_plot <- renderPlot({
    cd <- companies_data()
    if (nrow(cd) == 0) return()
    
    if (input$rating_type == "best") {
      cd <- cd %>% arrange(desc(avg_rating)) %>% head(15)
      title_text <- "Top Compañías con Mejor Rating"
      hjust_val <- -0.2
      fill_colors <- c("#66cc99", "#006633") 
    } else {
      cd <- cd %>% arrange(avg_rating) %>% head(15)
      title_text <- "Top Compañías con Peor Rating"
      hjust_val <- 1.2
      fill_colors <- c("#ff9999", "#cc0000")
    }
    
    cd <- cd %>% 
      mutate(developer = factor(developer, levels = developer[order(avg_rating)]))
    
    ggplot(cd, aes(x = developer, y = avg_rating, fill = avg_rating)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = round(avg_rating, 1)), hjust = hjust_val, size = 4, color = "black") +
      scale_fill_gradient(low = fill_colors[1], high = fill_colors[2]) +
      labs(x = NULL, y = "Rating Promedio", title = NULL) +
      coord_flip() +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      ) +
      scale_y_continuous(limits = c(0, 5), expand = expansion(mult = c(0, 0.1)))
  })
  
  output$company_details <- renderDT({
    req(input$min_games)
    
    prepared_data %>%
      group_by(developer) %>%
      summarise(
        "Juegos Publicados" = n(),
        "Rating Promedio" = round(mean(rating, na.rm = TRUE), 2),
        "Descargas Totales (millones)" = round(sum(estimated_downloads, na.rm = TRUE) / 1e6, 1)
      ) %>%
      filter(`Juegos Publicados` >= input$min_games) %>%
      arrange(desc(`Juegos Publicados`)) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE, dom = 'lftip'),
        rownames = FALSE
      )
  })
  
  output$difficulty_pie <- renderPlot({
    difficulty_data <- prepared_data %>%
      group_by(difficulty) %>%
      summarise(count = n()) %>%
      mutate(percentage = count/sum(count)*100)
    
    ggplot(difficulty_data, aes(x = "", y = count, fill = difficulty)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = NULL, fill = "Dificultad") +
      theme_void() +
      scale_fill_brewer(palette = "Set3") +
      theme(legend.position = "right")
  })
  
  output$length_pie <- renderPlot({
    length_data <- prepared_data %>%
      group_by(length) %>%
      summarise(count = n()) %>%
      mutate(percentage = count/sum(count)*100)
    
    ggplot(length_data, aes(x = "", y = count, fill = length)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = NULL, fill = "Duración") +
      theme_void() +
      scale_fill_brewer(palette = "Pastel1") +
      theme(legend.position = "right")
  })
  
  output$age_pie <- renderPlot({
    age_data <- prepared_data %>%
      group_by(age_restriction) %>%
      summarise(count = n()) %>%
      mutate(percentage = count/sum(count)*100)
    
    ggplot(age_data, aes(x = "", y = count, fill = age_restriction)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = NULL, fill = "Edad") +
      theme_void() +
      scale_fill_brewer(palette = "Pastel2") +
      theme(legend.position = "right")
  })
  
  output$os_pie <- renderPlot({
    ggplot(os_data, aes(x = "", y = Count, fill = OS)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(Count/sum(Count)*100, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 3.5) +
      labs(title = NULL, fill = "SO") +
      theme_void() +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.position = "right")
  })
}

# 5) Ejecutar app
shinyApp(ui = ui, server = server)

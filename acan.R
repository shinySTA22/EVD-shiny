library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Fungsi untuk membuat plot dengan garis regresi, garis smoothing, dan garis residual
create_regression_plot <- function(data, x_var, y_var, smoothness, show_cor, show_r_squared, show_reg_line, show_smooth_line, show_residuals, show_residual_sd) {
  
  # Membuat model regresi linear
  lm_model <- lm(formula = paste(y_var, "~", x_var), data = data)
  
  # Membuat ggplot dengan garis regresi dan garis smoothing
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    theme_minimal()
  
  # Menambahkan garis regresi jika show_reg_line TRUE
  if (show_reg_line) {
    p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
  }
  
  # Menambahkan garis smoothing jika show_smooth_line TRUE
  if (show_smooth_line) {
    p <- p + geom_smooth(method = "loess", se = FALSE, color = "blue", span = smoothness)
  }
  
  # Menambahkan garis residuals jika show_residuals TRUE
  if (show_residuals) {
    residuals <- residuals(lm_model)
    data$residuals <- residuals
    p <- p + geom_segment(aes(xend = data[[x_var]], yend = predict(lm_model)), color = "green")
  }
  
  # Menampilkan standar deviasi dari residuals jika show_residual_sd TRUE
  if (show_residual_sd) {
    residual_sd <- sd(residuals)
    p <- p +
      annotate("text", x = 0.02, y = -0.40, label = paste("<b>Residual SD:</b> ", round(residual_sd, 3)),
               parse = TRUE, hjust = 0, vjust = 0, size = 3)
  }
  
  # Menambahkan R-squared jika show_r_squared TRUE
  if (show_r_squared) {
    r_squared_value <- round(summary(lm_model)$r.squared * 100, 2) # mengonversi ke persen
    p <- p +
      annotate("text", x = 0.02, y = -0.45, label = paste("<b>R-squared:</b> ", r_squared_value, "%"),
               parse = TRUE, hjust = 0, vjust = 0, size = 3)
  }
  
  # Menambahkan koefisien korelasi jika show_cor TRUE
  if (show_cor) {
    cor_value <- round(cor(data[[x_var]], data[[y_var]]), 3)
    p <- p +
      annotate("text", x = 0.02, y = -0.28, label = "<b>Korelasi:</b>", parse = TRUE, hjust = 0, vjust = 0, size = 3) +
      annotate("text", x = 0.02, y = -0.35, label = paste("<b>Cor:</b> ", cor_value), parse = TRUE, hjust = 0, vjust = 0, size = 3)
  }
  
  # Mengonversi ggplot menjadi plotly
  plotly_output <- ggplotly(p)
  
  # Menghilangkan teks di dalam layout grafik jika checkbox diaktifkan
  if (!(show_cor || show_r_squared || show_residual_sd)) {
    plotly_output <- plotly_output %>% 
      layout(annotations = NULL)
  }
  
  return(plotly_output)
}

# Fungsi untuk membuat data sesuai jenis dan jumlah observasi
create_random_data <- function(n, type) {
  set.seed(123)
  if (type == "Linear Data") {
    x <- runif(n, 0, 10)
    y <- 2 * x + rnorm(n, mean = 0, sd = 5)
  } else if (type == "Quadratic Data") {
    x <- runif(n, 0, 10)
    y <- 3 * x^2 + rnorm(n, mean = 0, sd = 5)
  } else {
    x <- runif(n, 0, 10)
    y <- runif(n, 0, 50)
  }
  return(data.frame(x = x, y = y))
}

# Fungsi UI
ui <- fluidPage(
  titlePanel("Explorasi Regresi Linear"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataOption", "Pilih Jenis Data", c("Random Data", "Linear Data", "Quadratic Data")),
      selectInput("numObservations", "Jumlah Observasi", c(20, 50, 100, 500)),
      checkboxInput("showCor", "Tampilkan Korelasi", value = FALSE),
      checkboxInput("showRSquared", "Tampilkan R-squared", value = FALSE),
      checkboxInput("showRegLine", "Tampilkan Garis Regresi", value = FALSE),
      checkboxInput("showSmoothLine", "Tampilkan Smoothing Line", value = FALSE),
      conditionalPanel(
        condition = "input.showSmoothLine == true",
        sliderInput("smoothness", "Smoothness:", min = 0, max = 1, value = 0.2, step = 0.1)  # Mengubah nama slicer
      ),
      checkboxInput("showResiduals", "Tampilkan Residuals", value = FALSE),
      checkboxInput("showResidualSD", "Tampilkan SD Residuals", value = FALSE)
    ),
    mainPanel(
      plotlyOutput("regressionPlot"),
      htmlOutput("correlationText")
    )
  )
)

# Fungsi Server
server <- function(input, output) {
  # Memanggil fungsi create_regression_plot dengan data x dan y sesuai jenis data dan jumlah observasi yang dipilih
  data <- reactive({
    create_random_data(input$numObservations, input$dataOption)
  })
  
  output$regressionPlot <- renderPlotly({
    # Memanggil fungsi create_regression_plot dan merender plotly
    create_regression_plot(data(), "x", "y", input$smoothness, input$showCor, input$showRSquared, input$showRegLine, input$showSmoothLine, input$showResiduals, input$showResidualSD)
  })
  
  # Menampilkan teks korelasi di bawah grafik
  output$correlationText <- renderUI({
    if (input$showCor || input$showRSquared || input$showResidualSD) {
      cor_value <- round(cor(data()$x, data()$y), 3)
      r_squared_value <- round(summary(lm(data()$y ~ data()$x))$r.squared * 100, 2) # mengonversi ke persen
      residual_sd <- sd(data()$y - predict(lm(data()$y ~ data()$x)))
      
      text_output <- "<p style='text-align:left; margin-left:10px; margin-top:10px;'>"
      if (input$showCor) {
        text_output <- paste(text_output, "<b>Korelasi:</b> ", cor_value, "<br>")
      }
      if (input$showRSquared) {
        text_output <- paste(text_output, "<b>R-squared:</b> ", r_squared_value, "%<br>")
      }
      if (input$showResidualSD) {
        text_output <- paste(text_output, "<b>Residual SD:</b> ", round(residual_sd, 3), "<br>")
      }
      text_output <- paste(text_output, "</p>")
      
      HTML(text_output)
    }
  })
}

# Menjalankan aplikasi Shiny
shinyApp(ui = ui, server = server)
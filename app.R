library(shiny)
library(ggplot2)
library(plotly)
library(gridlayout)
library(bslib)

## --------------------------------------- UI ---------------------------------------
ui <- shinyUI(
    fluidPage(
      titlePanel("Eksplorasi Regresi Linier"),
      sidebarLayout(
        # sidebar
        sidebarPanel(
            # sidebar
            selectInput(inputId = "data", "Pilih data", choices = c("Input Mandiri", "Bilangan Acak", "Dataset Linier", "Dataset Kuadratik")),
            # ----- Input Mandiri
            conditionalPanel(
                condition = "input.data == 'Input Mandiri'",
                textOutput(outputId = "caption")
            ),
            # ----- Bilangan Acak, Dataset Linier, Dataset Kuadratik
            conditionalPanel(
                condition = "input.data == 'Bilangan Acak' || input.data == 'Dataset Linier' || input.data == 'Dataset Kuadratik'",
                sliderInput(inputId = "slider.n", label = "Pilih ukuran data", min = 10, max = 300, value = 100)
            ),
            # ----- Data Linier, Kuadratik
            conditionalPanel(
                condition = "input.data == 'Dataset Linier' || input.data == 'Dataset Kuadratik'",
                sliderInput(inputId = "slope", label = "Tentukan Kemiringan/Slope", min = -5, max = 5, value = 0, step = 0.05),
                radioButtons(inputId = "spread", label = "Sebaran data", choices = c("kecil", "sedang", "besar")),
            ),

            # ----- Refresh Button
            actionButton("refresh", "Refresh"),
            # ----- Pilihan-pilihan
            h3(""),
            checkboxInput(inputId = "reg", label = "Garis Regresi Linier"),
            checkboxInput(inputId = "smt", label = "Garis Tren Pemulusan"),
            conditionalPanel(
                condition = "input.smt == true",
                sliderInput(inputId = "slider.smooth", label = "Pilih ukuran pemulusan", min = 0, max = 1, value = 0.5)
            ),
            checkboxInput(inputId = "r", label = "Koefisien Korelasi (r)"),
            checkboxInput(inputId = "r2", label = "Kuadrat Koefisien Korelasi (r²)"),
            checkboxInput(inputId = "res", label = "Visualisasi Galat"),
            checkboxInput(inputId = "std", label = "Simpangan Baku Galat"),
            checkboxInput(inputId = "zsc", label = "Standardisasi Peubah"),
            conditionalPanel(
                condition = "input.data == 'Input Mandiri'",
                sliderInput(inputId = "slider.x", label = "Atur rentang X", min = -100, max = 100, value = c(-10, 10)),
                sliderInput(inputId = "slider.y", label = "Atur rentang Y", min = -100, max = 100, value = c(-10, 10))
            ),
            actionButton(inputId = "show_sum", "Show Summary")
        ),
        # main
        mainPanel(
            tabsetPanel(
                #tab 1
                tabPanel(
                    #plot
                    title = "Plot",
                    plotlyOutput(outputId = "plot", width = "100%", height = "100%"),
                    #tabel
                    tableOutput(outputId = "table")
                ),
                tabPanel(
                    # information
                    title = "Information",
                    textOutput(outputId = "info")
                )
            )
        )
      )
    )
)

## -------------------------------------- DATA --------------------------------------

#### Generate Random Data

generateRandomData <- function(n, type, s, slope) {
    set.seed(rpois(1, 10000))
    if(s == "kecil") {
        sd = 2
        } else if (s == "besar") {
            sd = 15
        } else {
            sd = 5
        }
    x <- runif(n, 0, 10)
    if(type == "Dataset Linier") {
        y <- slope * x + rnorm(n, 0, sd)
    } else if (type == "Dataset Kuadratik") {
        y <- slope * x^2 + rnorm(n, 0, sd)
    } else {
        y <- runif(1, -5, 5) * x + runif(n, 0, 10)
    }
    return(data.frame(x = x, y = y))
}

#### Create Regression Plot

createRegressionPlot <- function(data, x_var, y_var, smoothness, show_reg_line, show_smooth_line, show_residuals) {
    # -- model regresi
    lm_model <- lm(formula = paste(y_var, "~", x_var), data = data)

    # -- ggplot regresi
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_point() + theme_minimal()

    # -- ggplot regression line = T
    if (show_reg_line) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "#5959b8")
        }

    # -- ggplot smooth line = T
    if (show_smooth_line) {
        p <- p + geom_smooth(method = "loess", se = FALSE, color = "#92fb51", span = smoothness)
        }

    # -- ggplot show residuals = T
    if (show_residuals) {
        residuals <- residuals(lm_model)
        data$residuals <- residuals
        p <- p + geom_segment(aes(xend = data[[x_var]], yend = predict(lm_model)), color = "#f7f02b")
        }

    ## -- konversi ke plotly
    plotly_output <- ggplotly(p)

    return(plotly_output)
}

#### Append Dataframe Column

makeColumn <- function(col, val) {
    df <- data.frame(val)
    names(df) <- col
    return(df)
}


## ------------------------------------- SERVER -------------------------------------

server <- function(input, output) {

    ## -- data
    data <- reactive({
        req(input$data)
        if(input$data == "Bilangan Acak") {
            generateRandomData(n = input$slider.n, type = input$data, s = "sedang", slope = rnorm(1, mean = 0, sd = 5))
        } else if (input$data == "Dataset Linier" || input$data == "Dataset Kuadratik") {
            generateRandomData(n = input$slider.n, type = input$data, s = input$spread, slope = input$slope)
        } else {
            click <- event_data("plotly_click", source = "inputPlot")
            data.frame(x = click$x, y = click$y)
        }
    })

    # -- plot
    output$plot <- renderPlotly({
        if(input$data != "Input Mandiri"){
            if(input$zsc == TRUE) {
                x <- scale(data()$x)
                y <- scale(data()$y)
                data2 <- data.frame(x=x, y=y)
                createRegressionPlot(data = data2, x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res)
            } else {
                createRegressionPlot(data = data(), x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res)
            }
        } else {
            p <- ggplot() + xlim(input$slider.x[1], input$slider.x[2]) + ylim(input$slider.y[1], input$slider.y[2])
            ggplotly(p, source = "inputPlot")
        }
    })

    # -- table
    df1 <- reactive({
        if (input$r == TRUE){
            makeColumn("Korelasi X dan Y", cor(data()$x, data()$y))
        } else {
            data.frame("")
        }
    })
    df2 <- reactive({
        if (input$r2 == TRUE){
            makeColumn("Kuadrat Korelasi X dan Y", (cor(data()$x, data()$y))^2)
        } else {
            data.frame("")
        }
    })
    df3 <- reactive({
        if (input$std == TRUE){
            makeColumn("Simpangan Baku Galat", sd(data()$y - predict(lm(data()$y ~ data()$x))))
        } else {
            data.frame("")
        }
    })

    output$table <- renderTable(cbind(df1(), df2(), df3()))

}

shinyApp(ui, server)

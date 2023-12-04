library(shiny)
library(ggplot2)
library(plotly)
library(gridlayout)
library(bslib)
library(MASS)

## --------------------------------------- UI ---------------------------------------
ui <- shinyUI(
    fluidPage(
      titlePanel("Eksplorasi Regresi Linier"),
      sidebarLayout(
        # sidebar
        sidebarPanel(
            # sidebar
            selectInput(inputId = "data", "Pilih data", choices = c("Input Mandiri", "Dataset Acak", "Dataset Bangkitan Linier", "Dataset Bangkitan Kuadratik", "Dataset Kasus R")),
            # ----- Input Mandiri
            conditionalPanel(
                condition = "input.data == 'Input Mandiri'",
                textOutput(outputId = "caption")
            ),
            # ----- Dataset Acak, Dataset Bangkitan Linier, Dataset Bangkitan Kuadratik
            conditionalPanel(
                condition = "input.data == 'Dataset Acak' || input.data == 'Dataset Bangkitan Linier' || input.data == 'Dataset Bangkitan Kuadratik'",
                sliderInput(inputId = "slider.n", label = "Pilih ukuran data", min = 10, max = 300, value = 100)
            ),
            # ----- Data Linier, Kuadratik
            conditionalPanel(
                condition = "input.data == 'Dataset Bangkitan Linier' || input.data == 'Dataset Bangkitan Kuadratik'",
                sliderInput(inputId = "slope", label = "Tentukan Kemiringan/Slope", min = -5, max = 5, value = 0, step = 0.05),
                radioButtons(inputId = "spread", label = "Sebaran data", choices = c("kecil", "sedang", "besar")),
            ),
            # ----- Dataset dari R
            conditionalPanel(
                condition = "input.data == 'Dataset Kasus R'",
                selectInput(inputId = "kasus.R", "Pilih Kasus", choices = c("cars", "mtcars", "women", "trees"))
            ),

            # ----- Refresh Button
            conditionalPanel(
                condition = "input.data != 'Dataset Kasus R'",
                actionButton("refresh", "Refresh")
            ),
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
            checkboxInput(inputId = "std", label = "Simpangan Baku Galat"),
            checkboxInput(inputId = "res", label = "Visualisasi Galat"),
            checkboxInput(inputId = "zsc", label = "Standardisasi Peubah"),
            conditionalPanel(
                condition = "input.data == 'Input Mandiri'",
                sliderInput(inputId = "slider.x", label = "Atur rentang X", min = -100, max = 100, value = c(-10, 10)),
                sliderInput(inputId = "slider.y", label = "Atur rentang Y", min = -100, max = 100, value = c(-10, 10))
            ),
            actionButton(inputId = "show_sum", "Tampilkan Ringkasan"),
            h5(""),
            actionButton(inputId = "show_res", "Tampilkan Galat"),
            conditionalPanel(
                condition = "(input.show_res % 2) == 1",
                radioButtons(inputId = "type.res", "Jenis Galat", choices = c("Raw", "Studentized")),
                radioButtons(inputId = "plot.res", "Jenis Plot", choices = c("Galat vs X", "Galat vs Y duga")),
                checkboxInput(inputId = "smtres", "Garis Pemulusan"),
                conditionalPanel(
                    condition = "input.smtres == true",
                    sliderInput(inputId = "res_smooth", label = "Pilih ukuran pemulusan", min = 0, max = 1, value = 0.5)
                ),
                checkboxInput(inputId = "histres", "Histogram Galat"),
                conditionalPanel(
                    condition = "input.histres == true",
                    sliderInput(inputId = "bin", "Tentukan Lebar Bin", min = 1, max = 20, step = 0.1, value = 5),
                    checkboxInput(inputId = "curve", "Tampilkan Kurva Normal")
                )
            )
        ),
        # main
        mainPanel(
            tabsetPanel(
                #tab 1
                tabPanel(
                    #plot
                    title = "Data",
                    plotlyOutput(outputId = "plot", width = "100%", height = "100%"),
                    #tabel
                    tableOutput(outputId = "table"),
                    conditionalPanel(
                        condition = "(input.show_sum % 2) == 1",
                        verbatimTextOutput(outputId = "summary", placeholder = FALSE)
                    ),
                    conditionalPanel(
                        condition = "(input.show_res % 2) == 1",
                        plotlyOutput(outputId = "resplot", width = "100%", height = "100%")
                    )
                ),
                tabPanel(
                    # information
                    title = "Informasi",
                    verbatimTextOutput(outputId = "txt", placeholder = FALSE)
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
    if(type == "Dataset Bangkitan Linier") {
        y <- slope * x + rnorm(n, 0, sd)
    } else if (type == "Dataset Bangkitan Kuadratik") {
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

    return(p)
}

createResidualPlot <- function(data, x_var, y_var, smoothness, show_smooth_line) {

    p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_point() + theme_minimal()

    if (show_smooth_line) {
        p <- p + geom_smooth(method = "loess", se = FALSE, color = "#92fb51", span = smoothness)
        }
    
    return(p)
}

#### Append Dataframe Column

makeColumn <- function(col, val) {
    df <- data.frame(val)
    names(df) <- col
    return(df)
}


## ------------------------------------- SERVER -------------------------------------

server <- function(input, output, session) {

    ## -- data
    get.data <- reactive({
        switch(input$kasus.R,
           "cars" = data.frame(x = cars$speed, y = cars$dist),
           "women" = data.frame(x = women$height, y = women$weight),
           "mtcars" = data.frame(x = mtcars$hp, y = mtcars$mpg),
           "trees" = data.frame(x = trees$Height, y = trees$Girth))
    })

    data <- reactive({
        req(input$data)

        # refresh
        # input$refresh

        if(input$data == "Dataset Acak") {
            generateRandomData(n = input$slider.n, type = input$data, s = "sedang", slope = rnorm(1, mean = 0, sd = 5))
        } else if (input$data == "Dataset Bangkitan Linier" || input$data == "Dataset Bangkitan Kuadratik") {
            generateRandomData(n = input$slider.n, type = input$data, s = input$spread, slope = input$slope)
        } else if (input$data == "Dataset Kasus R"){
            ### -----
            get.data()
            ### ---
        } else {
            d <- event_data("plotly_click", source = "plot_click")
            df <- data.frame(x = numeric(), y = numeric())
            if(is.null(d)) {
                df
            } else {
                df <- rbind(df, data.frame(x = d$x, y = d$y))
                df
            }
        }
    })

    residual <- reactive({
        model.reg <- lm(y ~ x, data())
        res <- residuals(model.reg)
        stdres <- studres(model.reg)
        pred <- predict(model.reg, data())

        if (input$type.res == "Raw"){
            y <- res
        } else {
            y <- stdres
        }
        if (input$plot.res == "Galat vs X") {
            x <- data()$x
        } else {
            x <- pred
        }
        data.frame(x = x, y = y)
    })

    # -- plot
    output$plot <- renderPlotly({
        if(input$data != "Input Mandiri"){
            if(input$zsc == TRUE) {
                x <- scale(data()$x)
                y <- scale(data()$y)
                data2 <- data.frame(x=x, y=y)
                ggplotly(createRegressionPlot(data = data2, x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res))
            } else {
                ggplotly(createRegressionPlot(data = data(), x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res))
            }
        } else {
            ggplotly(ggplot(data(), aes(x = x, y = y)) + geom_point() + lims(x = c(input$slider.x[1], input$slider.x[2]), y = c(input$slider.y[1], input$slider.y[2])), source = "plot_click")
        }
    })

    output$resplot <- renderPlotly({
        ggplotly(createResidualPlot(residual(), "x", "y", smoothness = input$res_smooth, show_smooth_line = input$smtres))
    })

    # -- table
    df <- reactive({
        tab <- makeColumn("Ukuran Data", nrow(data()))
        if(input$r == TRUE){
            tab <- cbind(tab, makeColumn("Korelasi X dan Y", cor(data()$x, data()$y)))
        }
        if(input$r2 == TRUE){
            tab <- cbind(tab, makeColumn("Kuadrat Korelasi X dan Y", (cor(data()$x, data()$y))^2))
        }
        if(input$std == TRUE){
            tab <- cbind(tab, makeColumn("Simpangan Baku Galat", sd(data()$y - predict(lm(data()$y ~ data()$x)))))
        }
        tab
    })

    output$table <- renderTable(df())

    output$summary <- renderPrint({
        model.lm <- lm(y~x, data = data())
        summary(model.lm)
    })

}

shinyApp(ui, server)

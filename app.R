library(shiny)
library(ggplot2)
library(plotly)
library(gridlayout)
library(bslib)
library(MASS)

source('helper.R')

stringTxt <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."

## --------------------------------------- UI ---------------------------------------
ui <- fluidPage(
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
                  radioButtons(inputId = "spread", label = "Sebaran data", choices = c("kecil", "sedang", "besar"))
              ),
              # ----- Dataset dari R
              conditionalPanel(
                  condition = "input.data == 'Dataset Kasus R'",
                  selectInput(inputId = "kasus_R", "Pilih Kasus", choices = c("cars", "mtcars", "women", "trees"))
              ),
              
              # ----- Input Mandiri
              conditionalPanel(
                  condition = "input.data == 'Input Mandiri'",
                  sliderInput(inputId = "slider.x", label = "Atur rentang X", min = -100, max = 100, value = c(-10, 10)),
                  sliderInput(inputId = "slider.y", label = "Atur rentang Y", min = -100, max = 100, value = c(-10, 10))
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
                      sliderInput(inputId = "bin", "Tentukan Lebar Bin", min = 1, max = 10, step = 0.1, value = 5),
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
                      conditionalPanel(
                          condition = "input.data != 'Input Mandiri'",
                          plotlyOutput(outputId = "plot", width = "100%", height = "100%")
                      ),
                      conditionalPanel(
                          condition = "input.data == 'Input Mandiri'",
                          plotOutput("plot2", click = "plot_click"),
                          actionButton("rem_point", "Batalkan Titik Terakhir")
                      ),
                      
                      #tabel
                      tableOutput(outputId = "table"),
                      conditionalPanel(
                          condition = "(input.show_sum % 2) == 1",
                          verbatimTextOutput(outputId = "summary", placeholder = FALSE)
                      ),
                      conditionalPanel(
                          condition = "(input.show_res % 2) == 1",
                          plotlyOutput(outputId = "resplot", width = "100%", height = "100%")
                      ),
                      conditionalPanel(
                          condition = "input.histres == true",
                          plotlyOutput(outputId = "histplot", width = "100%", height = "100%")
                      )
                  ),
                  tabPanel(
                      # information
                      title = "Informasi",
                      # input mandiri
                      conditionalPanel(
                          condition = "input.data == 'Input Mandiri'",
                          # textOutput("input_mandiri")
                          includeMarkdown("www/input-mandiri.md")
                      ),
                      # dataset acak
                      conditionalPanel(
                          condition = "input.data == 'Dataset Acak'",
                          textOutput("dataset_acak")
                      ),
                      # dataset bangkitan linier
                      conditionalPanel(
                          condition = "input.data == 'Dataset Bangkitan Linier'",
                          # textOutput("dataset_linier")
                          includeMarkdown("www/dataset-linier.md")
                      ),
                      # dataset kuadratik
                      conditionalPanel(
                          condition = "input.data == 'Dataset Bangkitan Kuadratik'",
                          textOutput("dataset_kuadratik")
                      ),
                      # dataset R - cars
                      conditionalPanel(
                          condition = "input.kasus_R == 'cars' && input.data == 'Dataset Kasus R'",
                          textOutput("CARS")
                      ),
                      # dataset R - mtcars
                      conditionalPanel(
                          condition = "input.kasus_R == 'mtcars' && input.data == 'Dataset Kasus R'",
                          textOutput("MTCARS")
                      ),
                      # dataset R - women
                      conditionalPanel(
                          condition = "input.kasus_R == 'women' && input.data == 'Dataset Kasus R'",
                          textOutput("WOMEN")
                      ),
                      # dataset R - trees
                      conditionalPanel(
                          condition = "input.kasus_R == 'trees' && input.data == 'Dataset Kasus R'",
                          textOutput("TREES")
                      ),
                      # garis regresi linier
                      conditionalPanel(
                          condition = "input.reg == true",
                          textOutput("REG")
                      ),
                      # garis smoothing
                      conditionalPanel(
                          condition = "input.smt == true",
                          textOutput("SMT")
                      ),
                      # R
                      conditionalPanel(
                          condition = "input.r == true",
                          textOutput("R")
                      ),
                      # R2
                      conditionalPanel(
                          condition = "input.r2 == true",
                          textOutput("R2")
                      ),
                      # Galat
                      conditionalPanel(
                          condition = "input.std == true || input.res == true",
                          textOutput("STDRES")
                      ),
                      # Standardisasi Peubah
                      conditionalPanel(
                          condition = "input.zsc == true",
                          textOutput("ZSC")
                      ),
                      # Plot Galat
                      conditionalPanel(
                          condition = "(input.show_res % 2) == 1",
                          textOutput("GALAT")
                      ),
                      # Plot Histogram
                      conditionalPanel(
                          condition = "input.histres == true",
                          textOutput("HIST")
                      )
                  )
              )
          )
        )
)


## ------------------------------------- SERVER -------------------------------------

server <- function(input, output, session) {

    ## -- data
    get.data <- reactive({
        switch(input$kasus_R,
           "cars" = data.frame(x = cars$speed, y = cars$dist),
           "women" = data.frame(x = women$height, y = women$weight),
           "mtcars" = data.frame(x = mtcars$hp, y = mtcars$mpg),
           "trees" = data.frame(x = trees$Height, y = trees$Girth))
    })

    values <- reactiveValues()
    values$DT <- data.frame(x = numeric(), y = numeric())
    observeEvent(input$plot_click, {
        add_row <- data.frame(x = input$plot_click$x,
                              y = input$plot_click$y)
        values$DT <- rbind(values$DT, add_row)
    })

    observeEvent(input$rem_point, {
        rem_row <- values$DT[-nrow(values$DT), ]
        values$DT <- rem_row
    })


    data <- reactive({
        req(input$data)

        # refresh
        input$refresh
        isolate(set.seed(rpois(1, 10000)))

        if(input$data == "Dataset Acak") {
            generateRandomData(n = input$slider.n, type = input$data, s = "sedang", slope = rnorm(1, mean = 0, sd = 5))
        } else if (input$data == "Dataset Bangkitan Linier" || input$data == "Dataset Bangkitan Kuadratik") {
            generateRandomData(n = input$slider.n, type = input$data, s = input$spread, slope = input$slope)
        } else if (input$data == "Dataset Kasus R"){
            ### -----
            get.data()
            ### ---
        } else {
            values$DT
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
        if(input$zsc == TRUE) {
            x <- scale(data()$x)
            y <- scale(data()$y)
            data2 <- data.frame(x=x, y=y)
            ggplotly(createRegressionPlot(data = data2, x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res))
        } else {
            ggplotly(createRegressionPlot(data = data(), x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res))
        }
    })

    output$plot2 <- renderPlot({
        if(nrow(data()) > 1) {
            createRegressionPlot(data(), x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res) + lims(x = c(input$slider.x[1], input$slider.x[2]), y = c(input$slider.y[1], input$slider.y[2]))
        } else {
            ggplot(data(), aes(x = x, y = y)) + lims(x = c(input$slider.x[1], input$slider.x[2]), y = c(input$slider.y[1], input$slider.y[2])) + theme_minimal()
        }
    })

    output$resplot <- renderPlotly({

        if (input$type.res == "Raw"){
            y_lab = "Raw Residual"
        } else {
            y_lab <- "Studentized Residual"
        }
        if (input$plot.res == "Galat vs X") {
            x_lab <- "X"
        } else {
            x_lab <- "Y Predict"
        }
        
        ggplotly(createResidualPlot(residual(), "x", "y", smoothness = input$res_smooth, show_smooth_line = input$smtres, x_lab, y_lab))
    })

    output$histplot <- renderPlotly({
        res <- data.frame(err = residuals(lm(y ~ x, data())))

        ggplotly(createHistogram(res, "err", bin = input$bin, show_normal_curve = input$curve))
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

    # -- text

    # output$input_mandiri <- renderText({ paste0("INPUT MANDIRI ----- ", stringTxt) })
    output$dataset_acak <- renderText({ paste0("DATASET BILANGAN ACAK ----- ", stringTxt) })
    # output$dataset_linier <- renderText({ paste0("DATASET LINIER ----- ", stringTxt) })
    output$dataset_kuadratik <- renderText({ paste0("DATASET KUADRATIK ----- ", stringTxt) })
    output$CARS <- renderText({ paste0("DATASET CARS ----- ", stringTxt) })
    output$MTCARS <- renderText({ paste0("DATASET MTCARS ----- ", stringTxt) })
    output$WOMEN <- renderText({ paste0("DATASET WOMEN ----- ", stringTxt) })
    output$TREES <- renderText({ paste0("DATASET TREES ----- ", stringTxt) })

    output$REG <- renderText({ paste0("GARIS REGRESI ----- ", stringTxt) })
    output$SMT <- renderText({ paste0("GARIS PEMULUSAN ----- ", stringTxt) })
    output$R <- renderText({ paste0("KOEFISIEN KORELASI ----- ", stringTxt) })
    output$R2 <- renderText({ paste0("KOEFISIEN KORELASI KUADRAT ----- ", stringTxt) })
    output$STDRES <- renderText({ paste0("SIMPANGAN BAKU GALAT ----- ", stringTxt) })
    output$ZSC <- renderText({ paste0("STANDARDISASI PEUBAH ----- ", stringTxt) })
    output$GALAT <- renderText({ paste0("VISUALISASI GALAT ----- ", stringTxt) })
    output$HIST <- renderText({ paste0("HISTOGRAM GALAT ----- ", stringTxt) })

}

shinyApp(ui=ui, server=server)

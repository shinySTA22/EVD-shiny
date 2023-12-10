library(shiny)
library(ggplot2)
library(plotly)
# library(gridlayout)
library(bslib)
library(MASS)
library(broom)

source('helper.R')

## --------------------------------------- UI ---------------------------------------
ui <- fluidPage(
    titlePanel("Eksplorasi Regresi Linier"),
    withMathJax(),
    sidebarLayout(

        # ---------------- SIDEBAR
        sidebarPanel(
            ## ------ DATA
            selectInput(inputId = "data", "Pilih data", choices = c("Input Mandiri", "Dataset Acak", "Dataset Buatan (Linier)", "Dataset Buatan (Kuadratik)", "Dataset Kasus R")),
            # ----- Input Mandiri
            conditionalPanel(
                condition = "input.data == 'Input Mandiri'",
                textOutput(outputId = "caption"),
                sliderInput(inputId = "slider.x", label = "Atur rentang X", min = -100, max = 100, value = c(-10, 10)),
                sliderInput(inputId = "slider.y", label = "Atur rentang Y", min = -100, max = 100, value = c(-10, 10)),
                actionButton("reset", "Reset")
                ),
            # ----- Dataset Acak, Dataset Buatan (Linier), Dataset Buatan (Kuadratik)
            conditionalPanel(
                condition = "input.data == 'Dataset Acak' || input.data == 'Dataset Buatan (Linier)' || input.data == 'Dataset Buatan (Kuadratik)'",
                sliderInput(inputId = "slider.n", label = "Tentukan banyak amatan", min = 10, max = 300, value = 100)
                ),
            # ----- Data Linier, Kuadratik
            conditionalPanel(
                condition = "input.data == 'Dataset Buatan (Linier)' || input.data == 'Dataset Buatan (Kuadratik)'",
                sliderInput(inputId = "slope", label = "Tentukan Kemiringan/Slope", min = -5, max = 5, value = 0, step = 0.05),
                radioButtons(inputId = "spread", label = "Sebaran galat pada data", choices = c("kecil", "sedang", "besar"))
                ),
            # ----- Dataset dari R
            conditionalPanel(
                condition = "input.data == 'Dataset Kasus R'",
                selectInput(inputId = "kasus_R", "Pilih Kasus", choices = c("Jarak dan Kecepatan Mobil", "Bahan Bakar Mobil", "Tinggi vs Berat Badan Wanita", "Pengukuran Ketebalan Pohon"))
                ),
                
            # ----- Refresh Button
            conditionalPanel(
                condition = "input.data != 'Dataset Kasus R' && input.data != 'Input Mandiri'",
                actionButton("refresh", "Refresh")
                ),
              
            ## ------ PILIHAN-PILIHAN
            h5(""),
            checkboxInput(inputId = "reg", label = "Garis Regresi Linier"),
            checkboxInput(inputId = "smt", label = "Garis Tren Pemulusan"),
            conditionalPanel(
                condition = "input.smt == true",
                sliderInput(inputId = "slider.smooth", label = "Pilih ukuran pemulusan", min = 0, max = 1, value = 0.5)
                ),
                checkboxInput(inputId = "r", label = "Koefisien Korelasi (r)"),
                checkboxInput(inputId = "r2", label = "Koefisien Determinasi (r²)"),
                checkboxInput(inputId = "std", label = "Simpangan Baku Galat"),
                checkboxInput(inputId = "res", label = "Visualisasi Galat"),
                checkboxInput(inputId = "zscX", label = "Standardisasi Peubah X"),
                checkboxInput(inputId = "zscY", label = "Standardisasi Peubah Y"),
                actionButton(inputId = "show_sum", "Tampilkan Ringkasan"),
                
                h5(""),
                selectInput(inputId = "show_res", "Pilih Jenis Tampilan Galat", choices = c("Pilih Salah Satu", "Plot Tebaran", "Histogram")),
                conditionalPanel(
                    condition = "input.show_res == 'Plot Tebaran'",
                    radioButtons(inputId = "type.res", "Jenis Galat", choices = c("Raw", "Studentized")),
                    radioButtons(inputId = "plot.res", "Jenis Plot", choices = c("Galat vs X", "Galat vs Y duga")),
                    checkboxInput(inputId = "smtres", "Garis Pemulusan"),
                    conditionalPanel(
                        condition = "input.smtres == true",
                        sliderInput(inputId = "res_smooth", label = "Pilih ukuran pemulusan", min = 0, max = 1, value = 0.5)
                        )),
                conditionalPanel(
                    condition = "input.show_res == 'Histogram'",
                    sliderInput(inputId = "bin", "Tentukan Lebar Bin", min = 0.1, max = 10, step = 0.1, value = 5),
                    checkboxInput(inputId = "curve", "Tampilkan Kurva Kepekatan Galat")
                    )
            ),
        # ---------------- MAIN
        mainPanel(
            tabsetPanel(
                ## ----- DATA
                tabPanel(
                    title = "Data",
                    dataTableOutput(outputId = "data_table"),
                    downloadButton("downTable", "Unduh Tabel (CSV)")
                    ),
                ## ----- ANALISIS
                tabPanel(
                    ## -- plot
                    title = "Analisis",
                    conditionalPanel(
                        condition = "input.data != 'Input Mandiri'",
                        plotlyOutput(outputId = "plot", width = "100%", height = "100%")
                        ),
                    conditionalPanel(
                        condition = "input.data == 'Input Mandiri'",
                        plotOutput("plot2", click = "plot_click"),
                        actionButton("rem_point", "Batalkan Titik Terakhir")
                        ),
                      
                    # -- tabel
                    tableOutput(outputId = "table"),
                    conditionalPanel(
                        condition = "(input.show_sum % 2) == 1",
                        tableOutput(outputId = "summary")
                        ),
                    conditionalPanel(
                        condition = "input.show_res == 'Plot Tebaran'",
                        plotlyOutput(outputId = "resplot", width = "100%", height = "100%")
                        ),
                    conditionalPanel(
                        condition = "input.show_res == 'Histogram'",
                        plotlyOutput(outputId = "histplot", width = "100%", height = "100%")
                        )
                    ),
                ## ----- INFORMASI
                tabPanel(
                    title = "Informasi",
                    
                    # input mandiri
                    conditionalPanel(
                        condition = "input.data == 'Input Mandiri'",
                        includeMarkdown("www/input_mandiri.md")
                        ),
                    # dataset acak
                    conditionalPanel(
                        condition = "input.data == 'Dataset Acak'",
                        includeMarkdown("www/dataset_acak.md")
                        ),
                    # daataset linier
                    conditionalPanel(
                        condition = "input.data == 'Dataset Buatan (Linier)'",
                        includeMarkdown("www/dataset_linier.md")
                        ),
                    # dataset kuadratik
                    conditionalPanel(
                        condition = "input.data == 'Dataset Buatan (Kuadratik)'",
                        includeMarkdown("www/dataset_kuadratik.md")
                        ),
                    # dataset R - cars
                    conditionalPanel(
                        condition = "input.kasus_R == 'Jarak dan Kecepatan Mobil' && input.data == 'Dataset Kasus R'",
                        includeMarkdown("www/CARS.md")
                        ),
                    # dataset R - mtcars
                    conditionalPanel(
                        condition = "input.kasus_R == 'Bahan Bakar Mobil' && input.data == 'Dataset Kasus R'",
                        includeMarkdown("www/MTCARS.md")
                        ),
                    # dataset R - women
                    conditionalPanel(
                        condition = "input.kasus_R == 'Tinggi vs Berat Badan Wanita' && input.data == 'Dataset Kasus R'",
                        includeMarkdown("www/WOMEN.md")
                        ),
                    # dataset R - trees
                    conditionalPanel(
                        condition = "input.kasus_R == 'Pengukuran Ketebalan Pohon' && input.data == 'Dataset Kasus R'",
                        includeMarkdown("www/TREES.md")
                        ),
                    # garis regresi linier
                    conditionalPanel(
                        condition = "input.reg == true",
                        includeMarkdown("www/garis_regresi.md")
                        ),
                    # garis pemulusan
                    conditionalPanel(
                        condition = "input.smt == true",
                        includeMarkdown("www/garis_pemulusan.md")
                        ),
                    # R
                    conditionalPanel(
                        condition = "input.r == true",
                        includeMarkdown("www/korelasi.md")
                        ),
                    # R2
                    conditionalPanel(
                        condition = "input.r2 == true",
                        includeMarkdown("www/determinasi.md")
                        ),
                    # Galat
                    conditionalPanel(
                        condition = "input.std == true || input.res == true",
                        includeMarkdown("www/simp_galat.md")
                        ),
                    # standardisaso
                      conditionalPanel(
                        condition = "input.zscX == true || input.zscY == true",
                        includeMarkdown("www/standardisasi.md")
                        ),
                    # scatterplot galat
                    conditionalPanel(
                        condition = "input.show_res == 'Plot Tebaran'",
                        includeMarkdown("www/visualisasi_galat.md")
                        ),
                    # histogram galat
                    conditionalPanel(
                        condition = "input.show_res == 'Histogram'",
                        includeMarkdown("www/histogram_galat.md")
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
            "Jarak dan Kecepatan Mobil" = data.frame(x = cars$speed, y = cars$dist),
            "Tinggi vs Berat Badan Wanita" = data.frame(x = women$height, y = women$weight),
            "Bahan Bakar Mobil" = data.frame(x = mtcars$hp, y = mtcars$mpg),
            "Pengukuran Ketebalan Pohon" = data.frame(x = trees$Height, y = trees$Girth))
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

    observeEvent(input$reset, {
        del_row <- data.frame(x = numeric(), y = numeric())
        values$DT <- del_row
    })

    data <- reactive({
        req(input$data)

        # refresh
        input$refresh
        isolate(set.seed(rpois(1, 10000)))

        if(input$data == "Dataset Acak") {
            generateRandomData(n = input$slider.n, type = input$data, s = "sedang", slope = rnorm(1, mean = 0, sd = 5))
        } else if (input$data == "Dataset Buatan (Linier)" || input$data == "Dataset Buatan (Kuadratik)") {
            generateRandomData(n = input$slider.n, type = input$data, s = input$spread, slope = input$slope)
        } else if (input$data == "Dataset Kasus R"){
            get.data()
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

        if(input$data != "Dataset Kasus R") {
            xlab <- "X / prediktor"
            ylab <- "Y / respon"
        } else {
            if(input$kasus_R == "Jarak dan Kecepatan Mobil"){
                xlab <- "Jarak tempuh (ft.)"
                ylab <- "Kecepatan (mil/jam)"
            } else if (input$kasus_R == "Bahan Bakar Mobil") {
                xlab <- "Tenaga kuda"
                ylab <- "Jarak tempuh bahan bakar (mil/galon)"
            } else if (input$kasus_R == "Tinggi vs Berat Badan Wanita") {
                xlab <- "Tinggi badan (inci)"
                ylab <- "Berat badan (pond)"
            } else {
                xlab <- "Tinggi kayu pohon (ft.)"
                ylab <- "Ketebalan pohon (inci)"
            }
        }

        data2 <- data()
        if (input$zscX == TRUE || input$zscY == TRUE) {
            if (input$zscX == TRUE) {
                data2$x <- scale(data2$x)
                xlab <- "X terstandardisasi"
            } 
            if (input$zscY == TRUE) {
                data2$y <- scale(data2$y)
                ylab <- "Y terstandardisasi"
            }
            ggplotly(createRegressionPlot(data = data2, x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res, x_lab = xlab, y_lab = ylab, size = 2.5))
        } else {
            ggplotly(createRegressionPlot(data = data(), x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res, x_lab = xlab, y_lab = ylab, size = 2.5))
        }
    })

    output$plot2 <- renderPlot({
        
        if(nrow(data()) > 0) {
            xlab <- "X / prediktor"
            ylab <- "Y / respon"
            data2 <- data()
            if (input$zscX == TRUE || input$zscY == TRUE) {
                if (input$zscX == TRUE) {
                    data2$x <- scale(data2$x)
                    xlab <- "X terstandardisasi"
                } 
                if (input$zscY == TRUE) {
                    data2$y <- scale(data2$y)
                    ylab <- "Y terstandardisasi"
                }

                createRegressionPlot(data2, x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res, x_lab = xlab, y_lab = ylab, size = 4) + lims(x = c(input$slider.x[1], input$slider.x[2]), y = c(input$slider.y[1], input$slider.y[2]))

            } else {
                createRegressionPlot(data(), x_var = "x", y_var = "y", smoothness = input$slider.smooth, show_reg_line = input$reg, show_smooth_line = input$smt, show_residuals = input$res, x_lab = "X / prediktor", y_lab = "Y / respon", size = 4) + lims(x = c(input$slider.x[1], input$slider.x[2]), y = c(input$slider.y[1], input$slider.y[2]))
            }            
            
        } else {
            ggplot(data(), aes(x = x, y = y)) + lims(x = c(input$slider.x[1], input$slider.x[2]), y = c(input$slider.y[1], input$slider.y[2])) + theme_minimal() + labs(y = "Y / respon", x = "X / prediktor")
        }
    })

    output$resplot <- renderPlotly({

        if (nrow(residual()) == 0) {
            dat <- data.frame(x = 0, y = 0)
        } else {
            dat <- residual()
        }

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
        
        ggplotly(createResidualPlot(dat, "x", "y", smoothness = input$res_smooth, show_smooth_line = input$smtres, x_lab, y_lab))
    })

    output$histplot <- renderPlotly({
        if (nrow(data()) == 0) {
            res <- data.frame(err = 0)
        } else {
            res <- data.frame(err = residuals(lm(y ~ x, data())))
        }

        ggplotly(createHistogram(res, "err", bin = input$bin, show_normal_curve = input$curve))
    })

    # -- table
    df <- reactive({
        tab <- makeColumn("Banyak Amatan", nrow(data()))
        if(input$r == TRUE){
            tab <- cbind(tab, makeColumn("Koefisien Korelasi (r)", cor(data()$x, data()$y)))
        }
        if(input$r2 == TRUE){
            tab <- cbind(tab, makeColumn("Koefisien Determinasi (r²)", paste(round((cor(data()$x, data()$y))^2*100, 2), "%")))
        }
        if(input$std == TRUE){
            tab <- cbind(tab, makeColumn("Simpangan Baku Galat", sd(data()$y - predict(lm(data()$y ~ data()$x)))))
        }
        tab
    })

    output$table <- renderTable(df())

    output$summary <- renderTable({
        model.lm <- lm(y~x, data = data())
        df <- glance(model.lm)[, c("r.squared", "sigma", "df", "df.residual", "statistic", "p.value", "AIC", "BIC")]
        colnames(df) <- c("R-squared", "SSE", "df", "Residual df", "t-statistic", "p-value", "AIC", "BIC")
        df
    })

    output$data_table <- renderDataTable({
        data()
    })

    # -- text

    output$caption <- renderText({
        "Arahkan kursor ke koordinat X dan Y yang diinginkan dan klik pada lokasi tersebut untuk merekam data."
    })

    # -- download

    output$downTable <- downloadHandler(
        filename = "data.csv",
        content = function(fname){
            write.csv(data(), fname)
            }
    )

}

shinyApp(ui=ui, server=server)


library(shiny)
library(plotly)
library(gridlayout)
library(bslib)


ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar plot  "
  ),
  theme = bs_theme(bootswatch = "minty"),
  row_sizes = c(
    "75px",
    "1fr"
  ),
  col_sizes = c(
    "30%",
    "1fr"
  ),
  gap_size = "0.7000000000000001rem",
  grid_card(
    area = "sidebar",
    card_body(
      selectInput(
        inputId = "chooseData",
        label = "Pilih Jenis Data",
        choices = list(
          "Masukkan Sendiri" = "Masukkan Sendiri",
          "Bilangan Acak" = "Bilangan Acak",
          "Data Linier" = "Data Linier",
          "Data Kuadratik" = "Data Kuadratik"
        ),
        selected = "Masukkan Sendiri",
        width = "100%"
      ),
      em("Pilih data sesuai dengan kebutuhan eksplorasi. Jika ingin menentukan titik data sendiri, arahkan mouse ke area plot dan klik pada koordinat yang dikehendaki."),
      actionButton(inputId = "resetButton", label = "Reset"),
      card(
                                                                                full_screen = TRUE,
                                                                                card_header("Opsi"),
                                                                                card_body(
                                                                                  checkboxGroupInput(
                                                                                    inputId = "checkboxGroup",
                                                                                    label = "Pilih minimal satu",
                                                                                    choices = list(
                                                                                      "Garis Regresi Linier" = "regline",
                                                                                      "Trend Pemulusan" = "smoothline",
                                                                                      "Koefisien Korelasi (r)" = "r",
                                                                                      "Koefisien Korelasi Kuadrat (r2)" = "r2",
                                                                                      "Galat" = "galat",
                                                                                      "Simpangan Baku Galat" = "sbgalat",
                                                                                      "Standardisasi Skor Z" = "zscore",
                                                                                      "Atur Rentang X" = "rangeX",
                                                                                      "Atur Rentang Y" = "rangeY"
                                                                                    ),
                                                                                    width = "100%"
                                                                                  )
                                                                                )
                                                                              )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Eksplorasi Regresi Linier",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot",
    card_header("Plot Tebaran Data"),
    card_body(
      plotlyOutput(
        outputId = "plotScatter",
        width = "100%",
        height = "100%"
      )
    ),
    card_footer(
      grid_container(
        layout = c(
          "inf1 inf2"
        ),
        row_sizes = c(
          "1fr"
        ),
        col_sizes = c(
          "1fr"
        ),
        gap_size = "10px",
        grid_card(area = "inf1", full_screen = TRUE),
        grid_card(area = "inf2", full_screen = TRUE)
      )
    )
  )
)


server <- function(input, output) {
  output$plotScatter <- renderPlotly({
    plot_ly(
      diamonds[diamonds$cut == input$chooseData,], 
      x = ~carat
    ) |> 
    add_histogram() 
  })
  
}

shinyApp(ui, server)

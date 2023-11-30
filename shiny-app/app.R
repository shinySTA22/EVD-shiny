library(shiny)
library(plotly)
library(gridlayout)
library(bslib)


ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar plot  ",
    "options plot  "
  ),
  row_sizes = c(
    "90px",
    "0.99fr",
    "1.01fr"
  ),
  col_sizes = c(
    "215px",
    "1fr"
  ),
  gap_size = "0.7000000000000001rem",
  grid_card(
    area = "sidebar",
    card_header("Opsi"),
    card_body(
      gap = "10px",
      selectInput(
        inputId = "data",
        label = "Pilih Data",
        choices = list(
          "Pilih Sendiri" = "choose_own",
          "Bilangan Acak" = "random_numbers",
          "Data Linier" = "linear",
          "Data Kuadratik" = "quadratic"
        ),
        selected = "choose_own",
        width = "100%"
      ),
      small(em("Pilih data sesuai dengan kebutuhan Anda untuk dieksplorasi secara regresi linier.")),
      actionButton(inputId = "resetButton", label = "Reset")
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
    card_header("Scatter Plot Data"),
    card_body(
      plotlyOutput(
        outputId = "plot_data",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "options",
    card_body(
      checkboxGroupInput(
        inputId = "myCheckboxGroup",
        label = "Pilihan",
        choices = list("choice a" = "a", "choice b" = "b")
      )
    )
  )
)


server <- function(input, output) {
   
  output$plot_data <- renderPlotly({
    plot_ly(
      diamonds[diamonds$cut == input$data,], 
      x = ~carat
    ) |> 
    add_histogram() 
  })
  
}

shinyApp(ui, server)
  


library(shiny)
library(ggplot2)
library(plotly)
library(gridlayout)
library(bslib)
library(MASS)

ui <- shinyUI({
  fluidPage(
    mainPanel(
      fluidRow(column(width = 6,
                        h4("Click plot to add points"),
                        actionButton("rem_point", "Remove Last Point"),
                        plotOutput("plot1", click = "plot_click")),
                 column(width = 6,
                        h4("Table of points on plot"),
                        tableOutput("table")))
    )
  )
})

server <- function(input, output){
    values <- reactiveValues()
    values$DT <- data.frame(x = numeric(),
                            y = numeric())

    output$plot1 = renderPlot({
       ggplot(values$DT, aes(x = x, y = y)) +
            geom_point(size = 5) +
            lims(x = c(0, 100), y = c(0, 100))
    })

    observeEvent(input$plot_click, {
        add_row <- data.frame(x = input$plot_click$x,
                              y = input$plot_click$y)
        values$DT <- rbind(values$DT, add_row)
    })

    observeEvent(input$rem_point, {
        rem_row <- values$DT[-nrow(values$DT), ]
        values$DT <- rem_row
    })

    output$table <- renderTable({
        values$DT
    })
}

shinyApp(ui, server)

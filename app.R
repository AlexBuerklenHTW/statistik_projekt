#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("bin_input", "Bin-Breite:", min = 1, max = 20, value = 1),
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server <- function(input, output) {
  output$histogram <- renderPlot({
    ggplot(data_1_clean, aes(x = age, fill = factor(survived))) +
      geom_histogram(binwidth = input$bin_input, color = "black") +
      scale_x_continuous(breaks = seq(0, max(data_1_clean$age, na.rm = TRUE), by = 1),
                         labels = seq(0, max(data_1_clean$age, na.rm = TRUE), by = 1)) +
      labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") +
      ggtitle("Aufteilung der Anzahl der Personen, die überlebt haben oder nicht") +
      scale_fill_manual(values = c("red", "green"), labels = c("Nicht-Überlebt", "Überlebt"), name = "Überlebensstatus")
  })
}

shinyApp(ui, server)







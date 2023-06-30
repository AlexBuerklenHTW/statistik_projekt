ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("bin_input", "Bin-Breite:", min = 1, max = 10, value = 1),
        selectInput("select_input", "Überlebensstatus auswählen:",
                    choices = c("Alle", "Überlebt", "Nicht-Überlebt"),
                    selected = "Alle")
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server <- function(input, output) {
  output$histogram <- renderPlot({
    bin_width <- input$bin_input
    switch(input$select_input,
          "Überlebt" = {ggplot(data_1_alive, aes(x = age, fill = factor(survived))) +
              geom_histogram(binwidth = bin_width, color = "black") +
              scale_x_continuous(breaks = seq(0, max(data_1_alive$age, na.rm = TRUE), by = 1),
                                 labels = seq(0, max(data_1_alive$age, na.rm = TRUE), by = 1)) +
              labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") + 
              scale_fill_manual(values = c("green"), labels = c("Überlebt"), name = "Überlebensstatus")},
    
          "Nicht-Überlebt" = {ggplot(data_1_dead, aes(x = age, fill = factor(survived))) +
              geom_histogram(binwidth = bin_width, color = "black") +
              scale_x_continuous(breaks = seq(0, max(data_1_dead$age, na.rm = TRUE), by = 1),
                                 labels = seq(0, max(data_1_dead$age, na.rm = TRUE), by = 1)) +
              labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") + 
              scale_fill_manual(values = c("red"), labels = c("Nicht-Überlebt"), name = "Überlebensstatus")},
          
          "Alle" = { ggplot(data_1, aes(x = age, fill = factor(survived))) +
              geom_histogram(binwidth = bin_width, color = "black") +
              scale_x_continuous(breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 1),
                                 labels = seq(0, max(data_1$age, na.rm = TRUE), by = 1)) +
              labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") +
              ggtitle("Aufteilung der Anzahl der Personen, die überlebt haben oder nicht") +
              scale_fill_manual(values = c("red", "green"), labels = c("Nicht-Überlebt", "Überlebt"), name = "Überlebensstatus")}
)
  })
}

shinyApp(ui, server)
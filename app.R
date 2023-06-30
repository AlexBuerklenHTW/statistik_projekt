library(shiny)
library(ggplot2)

data_1$Survived <- factor(data_1$Survived, labels = c("Nein", "Ja"))
data_1$age_group <- cut(data_1$Age, breaks = seq(0, max(data_1$Age, na.rm = TRUE), by = 10), include.lowest = TRUE, labels = FALSE)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("survived_input", "Überlebensstatus:",
                  choices = c("Alle", "Überlebt", "Nicht-Überlebt")),
      sliderInput("bin_input", "Bin-Breite:", min = 1, max = 10, value = 1)
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server <- function(input, output) {
  
  output$histogram <- renderPlot({
    filtered_data <- switch(input$survived_input,
                            "Alle" = data_1,
                            "Überlebt" = data_1[data_1$Survived == "Ja", ],
                            "Nicht-Überlebt" = data_1[data_1$Survived == "Nein", ])
    
    ggplot(filtered_data, aes(x = age_group, fill = factor(Survived))) +
      geom_histogram(binwidth = input$bin_input, color = "black") +
      scale_x_continuous(breaks = seq(0, max(data_1$age_group, na.rm = TRUE), by = 1),
                         labels = seq(0, max(data_1$age_group, na.rm = TRUE), by = 1)) +
      labs(x = "Altersgruppe", y = "Anzahl", fill = "Überlebensstatus") +
      ggtitle("Aufteilung der Personen (je nach Altersgruppe), die überlebt haben oder nicht") +
      scale_fill_manual(values = c("red", "green"),
                        labels = c("Nicht-Überlebt", "Überlebt"),
                        name = "Überlebensstatus")
  })
  
  observeEvent(input$survived_input, {
    if (input$survived_input == "Nicht-Überlebt") {
      output$histogram <- renderPlot({
        filtered_data <- data_1[data_1$Survived == "Nein", ]
        
        ggplot(filtered_data, aes(x = age_group, fill = factor(Survived))) +
          geom_histogram(binwidth = input$bin_input, color = "black") +
          scale_x_continuous(breaks = seq(0, max(data_1$age_group, na.rm = TRUE), by = 1),
                             labels = seq(0, max(data_1$age_group, na.rm = TRUE), by = 1)) +
          labs(x = "Altersgruppe", y = "Anzahl", fill = "Überlebensstatus") +
          ggtitle("Aufteilung der Personen (je nach Altersgruppe), die nicht überlebt haben") +
          scale_fill_manual(values = "red", labels = "Nicht-Überlebt", name = "Überlebensstatus")
      })
    } else if (input$survived_input == "Überlebt") {
      output$histogram <- renderPlot({
        filtered_data <- data_1[data_1$Survived == "Ja", ]
        
        ggplot(filtered_data, aes(x = age_group, fill = factor(Survived))) +
          geom_histogram(binwidth = input$bin_input, color = "black") +
          scale_x_continuous(breaks = seq(0, max(data_1$age_group, na.rm = TRUE), by = 1),
                             labels = seq(0, max(data_1$age_group, na.rm = TRUE), by = 1)) +
          labs(x = "Altersgruppe", y = "Anzahl", fill = "Überlebensstatus") +
          ggtitle("Aufteilung der Personen (je nach Altersgruppe), die überlebt haben") +
          scale_fill_manual(values = "green", labels = "Überlebt", name = "Überlebensstatus")
      })
    }
  })
}

shinyApp(ui, server)

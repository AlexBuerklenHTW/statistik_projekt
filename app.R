ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      HTML("<h3>Wählen Sie die Bedingung aus</h3>"),
      selectInput("bedingung_input", "Bedingung auswählen:",
                  choices = c("Bedingung 1: Einfluss Alter auf Überlebenschance",
                              "Bedingung 2: Einfluss Passagierklasse auf Überlebenschance",
                              "Bedingung 3: Korrelation zwischen Eltern/Kinder und Überlebensstatus",
                              "Bedingung 4: Aufteilung der Geschlechter nach Überlebensstatus",
                              "Bedingung 5: Korrelation zwischen dem Überlebensstatus und die Anzahl der Geschwister/Ehepaare"),
                  selected = NULL),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 1: Einfluss Alter auf Überlebenschance'",
        sliderInput("bin_input", "Bin-Breite:", min = 1, max = 10, value = 1),
        selectInput("select_input", "Überlebensstatus auswählen:",
                    choices = c("Alle", "Überlebt", "Nicht-Überlebt"),
                    selected = "Alle")
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 2: Einfluss Passagierklasse auf Überlebenschance'",
        radioButtons(inputId = "choices_pclass",
                     label = "Passagierklassen",
                     choices = c("Alle","Passagierklasse 1","Passagierklasse 2","Passagierklasse 3"),
                     selected = "Alle"),
        plotOutput(outputId = "choice_pclass_final")
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 3: Korrelation zwischen Eltern/Kinder und Überlebensstatus'"
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 4: Aufteilung der Geschlechter nach Überlebensstatus'"
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 5: Korrelation zwischen dem Überlebensstatus und die Anzahl der Geschwister/Ehepaare'"
      )
      
    ),
    mainPanel(
      plotOutput("plots")
    )
  )
)

server <- function(input, output) {
  output$plots <- renderPlot({
    choice <- input$bedingung_input
    
    if (choice == "Bedingung 1: Einfluss Alter auf Überlebenschance") {
      bin_width <- input$bin_input
      
      switch(input$select_input,
             "Überlebt" = {
               ggplot(data_1_alive, aes(x = age, fill = factor(survived))) +
                 geom_histogram(binwidth = bin_width, color = "black") +
                 scale_x_continuous(breaks = seq(0, max(data_1_alive$age, na.rm = TRUE), by = 1),
                                    labels = seq(0, max(data_1_alive$age, na.rm = TRUE), by = 1)) +
                 labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") + 
                 scale_fill_manual(values = c("green"), labels = c("Überlebt"), name = "Überlebensstatus")
             },
             "Nicht-Überlebt" = {
               ggplot(data_1_dead, aes(x = age, fill = factor(survived))) +
                 geom_histogram(binwidth = bin_width, color = "black") +
                 scale_x_continuous(breaks = seq(0, max(data_1_dead$age, na.rm = TRUE), by = 1),
                                    labels = seq(0, max(data_1_dead$age, na.rm = TRUE), by = 1)) +
                 labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") + 
                 scale_fill_manual(values = c("red"), labels = c("Nicht-Überlebt"), name = "Überlebensstatus")
             },
             "Alle" = {
               ggplot(data_1, aes(x = age, fill = factor(survived))) +
                 geom_histogram(binwidth = bin_width, color = "black") +
                 scale_x_continuous(breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 1),
                                    labels = seq(0, max(data_1$age, na.rm = TRUE), by = 1)) +
                 labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") +
                 ggtitle("Aufteilung der Anzahl der Personen, die überlebt haben oder nicht") +
                 scale_fill_manual(values = c("red", "green"), labels = c("Nicht-Überlebt", "Überlebt"), name = "Überlebensstatus")
             }
      )
    }
    else if(choice == "Bedingung 2: Einfluss Passagierklasse auf Überlebenschance"){
        choice_pclass <- input$choices_pclass
        
      if(choice_pclass == "Alle"){
          mosaicplot(data_4_table,xlab = "Passagierklasse", ylab ="Überlebt oder nicht",main="Korrelation zwischen der Passagierklasse und dem Überlebensstatus") 
      }
      else if(choice_pclass == "Passagierklasse 1"){
        ggplot(data_2_clean_drill_down_1, aes(x = factor(survived))) +
          geom_histogram(stat = "count", fill = c("red", "green")) +
          scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
          labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Aufteilung der Passagiere in der 1ten Klasse") +
          theme_minimal()
      }
      else if(choice_pclass == "Passagierklasse 2"){
        
        ggplot(data_2_clean_drill_down_2, aes(x = factor(survived))) +
          geom_histogram(stat = "count", fill = c("red", "green")) +
          scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
          labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Aufteilung der Passagiere in der 2ten Klasse") +
          theme_minimal()
      }
      else if(choice_pclass == "Passagierklasse 3"){
        ggplot(data_2_clean_drill_down_3, aes(x = factor(survived))) +
          geom_histogram(stat = "count", fill = c("red", "green")) +
          scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
          labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Aufteilung der Passagiere in der 3ten Klasse") +
          theme_minimal()
      }
    }
    else if(choice == "Bedingung 3: Korrelation zwischen Eltern/Kinder und Überlebensstatus"){
      survival_rates <- aggregate(survived ~ parch, data = data_3_clean, FUN = mean)
      ggplot(survival_rates, aes(x = factor(parch), y = survived)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Anzahl der Eltern/Kinder an Bord", y = "Überlebensrate", title = "Korrelation zwischen Eltern/Kinder und Überlebensstatus")
    }
    else if(choice == "Bedingung 4: Aufteilung der Geschlechter nach Überlebensstatus"){
      ggplot(data_5, aes(x = factor(survived), fill = sex)) +
        geom_bar() +
        labs(x = "Überlebt", y = "Anzahl der Personen", title = "Aufteilung der Geschlechter nach Überlebensstatus") +
        scale_fill_manual(values = c("male" = "red", "female" = "green")) +
        theme_minimal()
    }
    else if(choice == "Bedingung 5: Korrelation zwischen dem Überlebensstatus und die Anzahl der Geschwister/Ehepaare"){
      mosaicplot(data_6_table,xlab = "Geschwister/Ehepartner", ylab ="Überlebt oder nicht",main="Korrelation zwischen dem Überlebensstatus und die Anzahl der Geschwister/Ehepaare")
    }
  })
}

shinyApp(ui, server)

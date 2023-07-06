library(ggplot2)
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      HTML("<h3>Wählen Sie eine Bedingung aus</h3>"),
      selectInput("bedingung_input", "Bedingung auswählen:",
                  choices = c("Bedingung 1: Einfluss Alter auf Überlebenschance",
                              "Bedingung 2: Einfluss Passagierklasse auf Überlebenschance",
                              "Bedingung 3: Überlebensrate der Eltern/Kinder",
                              "Bedingung 4: Aufteilung der Geschlechter nach Überlebenschance",
                              "Bedingung 5: Einfluss Überlebensstatus auf die Anzahl der Geschwister/Ehepaare (Relative Häufigkeit)"),
                  selected = NULL),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 1: Einfluss Alter auf Überlebenschance'",
        selectInput("select_input", "Überlebensstatus auswählen:",
                    choices = c("Übersicht", "Überlebt", "Nicht-Überlebt", "Relative Häufigkeit", "Boxplot"),
                    selected = "Übersicht"),
        sliderInput("range",
                    label = "Range of interest (nur für Übersicht, Überlebt und nicht Überlebt)",
                    min = 0, max = max(data_1$age, na.rm = TRUE), value = c(0,max(data_1$age, na.rm = TRUE)))
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 2: Einfluss Passagierklasse auf Überlebenschance'",
        radioButtons(inputId = "choices_pclass",
                     label = "Passagierklassen",
                     choices = c("Übersicht","Passagierklasse 1 als absolute Häufigkeit","Passagierklasse 2 als absolute Häufigkeit",
                                 "Passagierklasse 3 als absolute Häufigkeit", "Passagierklasse 1 als relative Häufigkeit für Frauen und Männer",
                                 "Passagierklasse 2 als relative Häufigkeit für Frauen und Männer",
                                 "Passagierklasse 3 als relative Häufigkeit für Frauen und Männer"),
                     selected = "Übersicht"),
        plotOutput(outputId = "choice_pclass_final")
        
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 3: Überlebensrate der Eltern/Kinder'"
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 4: Aufteilung der Geschlechter nach Überlebenschance'",
        radioButtons(inputId = "choices_survived",
                     label = "Überlebt/Nicht Überlebt",
                     choices = c("Übersicht","Überlebt","Nicht Überlebt", "Relative Häufigkeit"),
                     selected = "Übersicht")
      ),
      conditionalPanel(
        condition = "input.bedingung_input == 'Bedingung 5: Korrelation zwischen dem Überlebensstatus und die Anzahl der Geschwister/Ehepaare (Relative Häufigkeit)'"
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
      choice_age <- input$select_input
      range <- input$range
      
      if(choice_age == "Übersicht"){
        ggplot(data_1, aes(x = age, fill = factor(survived))) +
          geom_histogram(binwidth = 1, color = "black") +
          scale_x_continuous(breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 1),
                             labels = seq(0, max(data_1$age, na.rm = TRUE), by = 1)) +
          labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") +
          ggtitle("Aufteilung der Anzahl der Personen, die überlebt haben oder nicht") +
          scale_fill_manual(values = c("red", "green"), labels = c("Nicht-Überlebt", "Überlebt")) +
          coord_cartesian(xlim = range)}
      
      else if (choice_age == "Nicht-Überlebt"){
               ggplot(data_1_dead, aes(x = age, fill = factor(survived))) +
                 geom_histogram(binwidth = 1, color = "black") +
                 scale_x_continuous(breaks = seq(0, max(data_1_dead$age, na.rm = TRUE), by = 1),
                                    labels = seq(0, max(data_1_dead$age, na.rm = TRUE), by = 1)) +
                 labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") + 
                 scale_fill_manual(values = c("red"), labels = c("Nicht-Überlebt"), name = "Überlebensstatus") +
                 coord_cartesian(xlim = range)
      }
      
      else if (choice_age == "Überlebt"){
        ggplot(data_1_alive, aes(x = age, fill = factor(survived))) +
          geom_histogram(binwidth = 1, color = "black") +
          scale_x_continuous(breaks = seq(0, max(data_1_alive$age, na.rm = TRUE), by = 1),
                             labels = seq(0, max(data_1_alive$age, na.rm = TRUE), by = 1)) +
          labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") + 
          scale_fill_manual(values = c("green"), labels = c("Überlebt"), name = "Überlebensstatus")  +
          coord_cartesian(xlim = range)
      }
      else if (choice_age == "Relative Häufigkeit") {
        ggplot(data_1_rel_hauf, aes(x = age, y = Freq, fill = survived)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(round(Freq, 1), "%")), size = 6, color = "black") +
          labs(x = "Altersgruppen", y = "Prozentuale Verteilung", fill = "Überlebt oder nicht") +
          scale_fill_manual(values = c("red", "green"), labels = c("Nicht-Überlebt", "Überlebt")) + 
          ggtitle("Verteilung der Überlebenschancen auf die Altersgruppen")
      }
      else if (choice_age == "Boxplot"){
        ggplot(data_1_clean, aes(x = factor(survived), y = age)) +
          geom_boxplot() +
          labs(x = "Survived", y = "Age") +
          ggtitle("Boxplots des Alters basierend auf Überlebensstatus") +
          geom_hline(yintercept = median(data_1_clean$age), linetype = "dashed", color = "red") +
          scale_y_continuous(breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 10)) +
          geom_text(aes(x = 1.5, y = median(data_1_clean$age), label = median(data_1_clean$age)),color = "red", vjust = -1) +
          theme_minimal()
      }
    }
    else if(choice == "Bedingung 2: Einfluss Passagierklasse auf Überlebenschance"){
        choice_pclass <- input$choices_pclass
        
      if(choice_pclass == "Übersicht"){
        ggplot(data_4_table_prop, aes(x = pclass, y = Freq, fill = survived)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(round(Freq, 1), "%")), size = 6, color = "white") +
          labs(x = "Passagierklasse", y = "Prozentuale Verteilung", fill = "Überlebt oder nicht") +
          ggtitle("Einfluss der Passagierklasse auf die Überlebenschance") +
          theme_minimal()
      }
      else if(choice_pclass == "Passagierklasse 1 als absolute Häufigkeit"){
        ggplot(data_2_clean_drill_down_1, aes(x = factor(survived))) +
          geom_histogram(stat = "count", fill = c("red", "green")) +
          scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
          labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Aufteilung der Passagiere in der 1ten Klasse") +
          theme_minimal()
      }
      else if(choice_pclass == "Passagierklasse 2 als absolute Häufigkeit"){
        
        ggplot(data_2_clean_drill_down_2, aes(x = factor(survived))) +
          geom_histogram(stat = "count", fill = c("red", "green")) +
          scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
          labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Aufteilung der Passagiere in der 2ten Klasse") +
          theme_minimal()
      }
      else if(choice_pclass == "Passagierklasse 3 als absolute Häufigkeit"){
        ggplot(data_2_clean_drill_down_3, aes(x = factor(survived))) +
          geom_histogram(stat = "count", fill = c("red", "green")) +
          scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
          labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Aufteilung der Passagiere in der 3ten Klasse") +
          theme_minimal()
      }
      else if (choice_pclass == "Passagierklasse 1 als relative Häufigkeit für Frauen und Männer"){
        ggplot(data_1_pclass_1, aes(x = Var1, y = Freq, fill = Var2)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(round(Freq, 1), "%")), size = 6, color = "white") +
          labs(x = "Geschlecht", y = "Prozentuale Verteilung", fill = "Überlebt oder nicht") +
          ggtitle("Einfluss des Geschlechts auf die Überlebenschance in der 1ten Klasse") +
          theme_minimal()
      }
      else if (choice_pclass == "Passagierklasse 2 als relative Häufigkeit für Frauen und Männer"){
        ggplot(data_1_pclass_2, aes(x = Var1, y = Freq, fill = Var2)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(round(Freq, 1), "%")), size = 6, color = "white") +
          labs(x = "Geschlecht", y = "Prozentuale Verteilung", fill = "Überlebt oder nicht") +
          ggtitle("Einfluss des Geschlechts auf die Überlebenschance in der 2ten Klasse") +
          theme_minimal()
      }
      else if (choice_pclass == "Passagierklasse 3 als relative Häufigkeit für Frauen und Männer"){
        ggplot(data_1_pclass_3, aes(x = Var1, y = Freq, fill = Var2)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(round(Freq, 1), "%")), size = 6, color = "white") +
          labs(x = "Geschlecht", y = "Prozentuale Verteilung", fill = "Überlebt oder nicht") +
          ggtitle("Einfluss des Geschlechts auf die Überlebenschance in der 3ten Klasse") +
          theme_minimal()
      }
    }
    else if(choice == "Bedingung 3: Überlebensrate der Eltern/Kinder"){
       ggplot(data_3_table_prop, aes(x = parch, y = Freq, fill = survived)) +
         geom_bar(stat = "identity") +
         geom_text(aes(label = paste0(round(Freq, 1), "%")), size = 3.5, color = "white") +
         labs(x = "Anzahl der Eltern/Kinder", y = "Prozentuale Verteilung", fill = "Überlebt oder nicht") +
         ggtitle("Verteilung der Eltern/Kinder und deren Überlebenschance") +
         theme_minimal()
    }
    else if(choice == "Bedingung 4: Aufteilung der Geschlechter nach Überlebenschance"){
        choice_survived <- input$choices_survived
        if(choice_survived == "Übersicht"){
          ggplot(data_5, aes(x = factor(survived), fill = sex)) +
            geom_bar() +
            labs(x = "Nicht Überlebt & Überlebt", y = "Anzahl der Personen", title = "Aufteilung der Geschlechter nach Überlebenschance") +
            scale_fill_manual(values = c("male" = "blue", "female" = "pink")) +
            theme_minimal()
        }
        else if(choice_survived == "Überlebt"){
          ggplot(data_5_clean_drill_down_ueberlebt, aes(x = factor(survived), fill = sex)) +
            geom_bar() +
            geom_text(aes(label = ..count..), stat = "count", vjust = 2) +
            labs(x = "Überlebt", y = "Anzahl der Personen", title = "Aufteilung der Geschlechter nach Überlebenschance") +
            scale_fill_manual(values = c("male" = "blue", "female" = "pink")) +
            theme_minimal()
          
        }
        else if(choice_survived == "Nicht Überlebt"){
          ggplot(data_5_clean_drill_down_nichtueberlebt, aes(x = factor(survived), fill = sex)) +
            geom_bar() +
            geom_text(aes(label = ..count..), stat = "count", vjust = -1) +
            labs(x = "Überlebt", y = "Anzahl der Personen", title = "Aufteilung der Geschlechter nach Überlebenschance") +
            scale_fill_manual(values = c("male" = "blue", "female" = "pink")) +
            theme_minimal()
        }
        else if(choice_survived == "Relative Häufigkeit"){
          ggplot(data_5_table_prop, aes(x = sex, y = Freq, fill = survived)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(Freq, 1), "%")), size = 6, color = "white") +
            labs(x = "Geschlecht", y = "Prozentuale Verteilung", fill = "Überlebt oder nicht") +
            ggtitle("Einfluss des Geschlechts auf die Überlebenschance") +
            theme_minimal()
        }
        }
    else if(choice == "Bedingung 5: Einfluss Überlebensstatus auf die Anzahl der Geschwister/Ehepaare (Relative Häufigkeit)"){
      mosaicplot(data_6_table,xlab = "Geschwister/Ehepartner", ylab ="Überlebt/nicht Überlebt",
                 main="Verteilung der Überlebensschance auf die Anzahl der Geschwister/Ehepaare",
                 color = c("red","green"))
      text(x = 0.2, y = 0.8, labels = "65.46%", pos = 4, cex = 1)
      text(x = 0.2, y = 0.2, labels = "34.54%", pos = 4, cex = 1)
      text(x = 0.7, y = 0.8, labels = "46.41%", pos = 4, cex = 1)
      text(x = 0.8, y = 0.2, labels = "53.59%", pos = 2, cex = 1)
      text(x = 0.83, y = 0.8, labels = "53.57%", pos = 4, cex = 1)
      text(x = 0.87, y = 0.2, labels = "46.43%", pos = 2, cex = 1)
      text(x = 0.87, y = 0.8, labels = "75.00%", pos = 4, cex = 1)
      text(x = 0.92, y = 0.1, labels = "25.00%", pos = 2, cex = 1)
      text(x = 0.9, y = 0.7, labels = "83.33%", pos = 4, cex = 1)
      text(x = 0.95, y = 0.06, labels = "16.67%", pos = 2, cex = 1)
      text(x = 0.95, y = 0.7, labels = "100.00%", pos = 4, cex = 1)
      text(x = 0.99, y = 0.2, labels = "0.00%", pos = 2, cex = 1)
    }
  })
}

shinyApp(ui, server)

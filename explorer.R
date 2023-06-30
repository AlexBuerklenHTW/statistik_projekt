# embarked: Port of embarkation -> S = Southhampton, C = Cherbourg, 
# Q = Queenstown. Ablauf: Southampton–Cherbourg–Queenstown–New York, New York–Plymouth–Cherbourg–Southampton
# ideen: 
# 1.) alter + survived -> hat das alter einen einfluss auf die
# überlebenschance?
# 2.) fare + survived + age -> vllt hat ein teureres ticket zu einer höheren 
# überlebenschance geführt? (bevorzugung von passagieren?)
# 3.) parch + survived -> hatten eltern und kinder eine höhere 
# überlebenschance?
# 4.) pclass + survived -> hatte die passagierklasse einfluss auf die 
# überlebenschance? 
# 5.) sex + survived -> hatte das geschlecht einfluss auf die 
# überlebenschance?
# 6.) sibsp + survived -> hatten geschwister/ehepaare eine höhere 
# überlebenschance?


# 1.)
data_1 <- data.frame(survived, age)
data_1_clean <- na.omit(data_1)
data_1



data_1$survived <- factor(data_1$survived, labels = c("Nein", "Ja"))
data_1$survived


data_1$age_group <- cut(data_1$age, breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 10), include.lowest = TRUE, labels = FALSE)
data_1$age_group


plot_1 <- ggplot(data_1, aes(x = age, fill = factor(survived))) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 1),
                     labels = seq(0, max(data_1$age, na.rm = TRUE), by = 1)) +
  labs(x = "Alter", y = "Anzahl", fill = "Überlebensstatus") + 
  scale_fill_manual(values = c("red", "green"), labels = c("Nicht-Überlebt", "Überlebt"), name = "Überlebensstatus")


"Kinder 50%, Erwachsene 38%, Ältere Erwachsene 26%"

# 2.) 
library(ggplot2)
#main diagramm
data_2 <- data.frame(fare, survived, age)
data_2_clean <- na.omit(data_2)

plot_2 <- ggplot(data_2_clean, aes(x = factor(pclass), y = fare, color = factor(survived))) +
  geom_point(shape = 19) +
  scale_x_discrete(labels = c("1", "2", "3")) +
  scale_y_continuous(breaks = seq(0, max(data_2_clean$fare), by = 50)) +
  scale_color_manual(values = c("red", "green"), labels = c("Nicht-Überlebt", "Überlebt")) +
  labs(x = "Passagierklasse", y = "Ticketkosten", title = "Einfluss der Passagierklasse und Ticketkosten auf die Überlebenschance") +
  theme_minimal()

#passagierklasse 1
data_2_clean_drill_down_1 <- subset(data_2_clean, pclass == 1)

ggplot(data_2_clean_drill_down_1, aes(x = factor(survived))) +
  geom_histogram(stat = "count", fill = c("red", "green")) +
  scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
  labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Anzahl der Passagiere vs. Überlebensstatus (Passagierklasse 1)") +
  theme_minimal()

#passagierklasse 2
data_2_clean_drill_down_2 <- subset(data_2_clean, pclass == 2)

ggplot(data_2_clean_drill_down_2, aes(x = factor(survived))) +
  geom_histogram(stat = "count", fill = c("red", "green")) +
  scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
  labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Anzahl der Passagiere vs. Überlebensstatus (Passagierklasse 1)") +
  theme_minimal()

#passagierklasse 3
data_2_clean_drill_down_3 <- subset(data_2_clean, pclass == 3)

ggplot(data_2_clean_drill_down_3, aes(x = factor(survived))) +
  geom_histogram(stat = "count", fill = c("red", "green")) +
  scale_x_discrete(labels = c("Nicht überlebt", "Überlebt")) +
  labs(x = "Überlebensstatus", y = "Anzahl der Passagiere", title = "Anzahl der Passagiere vs. Überlebensstatus (Passagierklasse 1)") +
  theme_minimal()


# 3.) 
"Eltern und Kinder hatten eine höhere Überlebenschance auf der Titanic. Die Datenanalyse zeigt, 
dass Passagiere, die als Eltern oder Kinder zusammen mit ihrer Familie an Bord waren , 
eine höhere Überlebensrate hatten im Vergleich zu Passagieren, die allein reisten.."

survival_rates <- aggregate(survived ~ parch, data = data_3_clean, FUN = mean)
 
data_3_clean <- ggplot(survival_rates, aes(x = factor(parch), y = survived)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Anzahl der Eltern/Kinder an Bord", y = "Überlebensrate", title = "Korrelation zwischen Eltern/Kinder und Überlebensstatus")


# 4.)
data_4 <- data.frame(pclass,survived)
data_4_clean <- na.omit(data_4)
data_4_table <- table(data_4_clean)
mosaicplot(data_4_table,xlab = "Passagierklasse", ylab ="Überlebt oder nicht",main="Mosaicplot")
#Aus irgendeinem Grund muss man den ganzen Befehl eingebenen (die Zeile über diesem Kommentar)


# 5.)
data_5 <- data.frame(sex,survived)

plot_5 <- ggplot(data_5, aes(x = survived, fill = sex)) +
  geom_bar() +
  labs(x = "Überlebt", y = "Anzahl der Personen") +
  scale_fill_manual(values = c("male" = "red", "female" = "green")) +
  theme_minimal()

'Dem Diagramm ist zu entnehmen, dass der Großteil der Männer (im Diagramm über 400 Männer) nicht überlebt haben, wobei die Mehrheit der Frauen überlebt haben.'
"74% von den Frauen hat überlebt und nur 18 Prozent der Männer "


# 6.)
data_6 <- data.frame(sibsp,survived,age)
data_6_clean <- na.omit(data_6)
data_6

plot_6 <- ggplot(data_6_clean,aes(x=age, y = sibsp, color = factor(survived) )) +
  geom_point(shape = 19) +
  scale_x_continuous(breaks = seq(0, max(data_6_clean$age), by = 10)) +
  scale_color_manual(values = c("red", "green"), labels = c("Nicht überlebt", "Überlebt")) +
  labs(x = "Alter", y = "geschwister/ehepaare", title = "geschwister/ehepaare vs. Überlebenschance (Überlebensstatus)") +
  theme_dark()
print(plot_6)
"Auch bei diesem Diagram wird es deutig, das geschwister/ehepaare eine höhere Chance hatten zu überleben."
#vielleicht kann man auch daraus ein mosaik plot machen?
#data_6 <- data.frame(sibsp,survived)
#data_6_clean <- na.omit(data_6)
#data_6_table <- table(data_6_clean)
#mosaicplot(data_6_table,xlab = "Geschwister/Ehepartner", ylab ="Überlebt oder nicht",main="Mosaicplot")


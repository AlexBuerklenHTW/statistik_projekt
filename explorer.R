# embarked: Port of embarkation -> S = Southhampton, C = Cherbourg, 
# Q = Queenstown. Ablauf: Southampton–Cherbourg–Queenstown–New York, New York–Plymouth–Cherbourg–Southampton
# ideen: 
# 1.) alter + survived -> hat das alter einen einfluss auf die
# überlebenschance?
# 2.) fare + survived + age -> vllt hat ein teureres ticket zu einer höheren 
# überlebenschance geführt? (bevorzugung von passagieren?)
# 3.) parch + survived + age -> hatten eltern und kinder eine höhere 
# überlebenschance?
# 4.) pclass + survived + age -> hatte die passagierklasse einfluss auf die 
# überlebenschance? 
# 5.) sex + survived -> hatte das geschlecht einfluss auf die 
# überlebenschance?
# 6.) sibsp + survived + age -> hatten geschwister/ehepaare eine höhere 
# überlebenschance?

# 1.)
data_1 <- data.frame(survived, age)
data_1

data_1$survived <- factor(data_1$survived, labels = c("Nein", "Ja"))
data_1$survived


data_1$age_group <- cut(data_1$age, breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 10), include.lowest = TRUE, labels = FALSE)
data_1$age_group


'options(max.print = 1000)'  
plot_1 <- ggplot(data_1, aes(x = factor(age_group), fill = survived)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_x_discrete(labels = paste0(seq(0, max(data_1$age, na.rm = TRUE), by = 10), "-", seq(9, max(data_1$age, na.rm = TRUE), by = 10))) +
  labs(x = "Alter", y = "Anzahl", fill = "Überleben") +
  ggtitle("Häufigkeit des Überlebensstatus nach Alter")
print(plot_1)

# 2.) 
library(ggplot2)

data_2 <- data.frame(fare, survived, age)
data_2_clean <- na.omit(data_2)

plot_2 <- ggplot(data_2_clean, aes(x = age, y = fare, color = factor(survived))) +
  geom_point(shape = 19) +
  scale_x_continuous(breaks = seq(0, max(data_2_clean$age), by = 10)) +
  scale_color_manual(values = c("red", "green"), labels = c("Nicht überlebt", "Überlebt")) +
  labs(x = "Alter", y = "Ticketkosten", title = "Alter vs. Ticketkosten (Überlebensstatus)") +
  theme_dark()
print(plot_2)

'Dem Diagramm ist zu entnehmen, dass durch ein teureres Ticket, die Überlebenschancen um ein kleines bisschen höher sein könnte, wobei es auch Ausnahmefälle gibt'

# 3.) 
data_3 <- data.frame(parch,survived,age)
data_3



# 4.)
data_4 <- data.frame(pclass,survived,age)
data_4_clean <- na.omit(data_4)
data_4


plot_4 <- ggplot(data_4_clean, aes(x = age, y = pclass, color = factor(survived))) +
  geom_point(shape = 19) +
  scale_x_continuous(breaks = seq(0, max(data_4_clean$age), by = 10)) +
  scale_color_manual(values = c("red", "green"), labels = c("Nicht überlebt", "Überlebt")) +
  labs(x = "Alter", y = "Passenger Class", title = "Alter vs. Ticketkosten (Überlebensstatus)") +
  theme_dark()
print(plot_4)

'Man erkennt, dass in der Passenger Class 3 viel mehr Menschen gestorben sind, als bei 2 und 1. In der Passenger Class 1 haben die meisten Menschen überlebt.'


# 5.)
data_5 <- data.frame(sex,survived)

ggplot(data_5, aes(x = survived, fill = sex)) +
  geom_bar() +
  labs(x = "Überlebt", y = "Anzahl der Personen") +
  scale_fill_manual(values = c("male" = "red", "female" = "green")) +
  theme_minimal()

'Dem Diagramm ist zu entnehmen, dass der Großteil der Männer (im Diagramm über 400 Männer) nicht überlebt haben, wobei die Mehrheit der Frauen überlebt haben.'




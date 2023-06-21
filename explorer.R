# embarked: Port of embarkation -> S = Southhampton, C = Cherbourg, 
# Q = Queenstown. Ablauf: Southampton–Cherbourg–Queenstown–New York, New York–Plymouth–Cherbourg–Southampton
# ideen: 
# 1.) alter + überlebt -> hat das alter einen einfluss auf die
# überlebenschance?
# 2.) fare + survived -> vllt hat ein teureres ticket zu einer höheren 
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
# Dataframe zusammenführen
data <- data.frame(survived, age)

# Überlebensstatus als Faktor konvertieren
data$survived <- factor(data$survived, labels = c("Nein", "Ja"))

# Alter in diskrete Gruppen einteilen
data$age_group <- cut(data$age, breaks = seq(0, max(data$age, na.rm = TRUE), by = 10), include.lowest = TRUE, labels = FALSE)

# Plot erstellen
plot_1 <- ggplot(data, aes(x = factor(age_group), fill = survived)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_x_discrete(labels = paste0(seq(0, max(data$age, na.rm = TRUE), by = 10), "-", seq(9, max(data$age, na.rm = TRUE), by = 10))) +
  labs(x = "Alter", y = "Anzahl", fill = "Überleben") +
  ggtitle("Häufigkeit des Überlebensstatus nach Alter")

# Plot anzeigen
print(plot_1)
# 2.) 


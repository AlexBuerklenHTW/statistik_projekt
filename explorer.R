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
# 5.) sex + survived + age -> hatte das geschlecht einfluss auf die 
# überlebenschance?
# 6.) sibsp + survived + age -> hatten geschwister/ehepaare eine höhere 
# überlebenschance?

# 1.)
# Dataframe zusammenführen
data_1 <- data.frame(survived, age)

# Überlebensstatus als Faktor konvertieren
data_1$survived <- factor(data_1$survived, labels = c("Nein", "Ja"))

# Alter in diskrete Gruppen einteilen
data_1$age_group <- cut(data_1$age, breaks = seq(0, max(data_1$age, na.rm = TRUE), by = 10), include.lowest = TRUE, labels = FALSE)

# Plot erstellen
plot_1 <- ggplot(data_1, aes(x = factor(age_group), fill = survived)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_x_discrete(labels = paste0(seq(0, max(data_1$age, na.rm = TRUE), by = 10), "-", seq(9, max(data_1$age, na.rm = TRUE), by = 10))) +
  labs(x = "Alter", y = "Anzahl", fill = "Überleben") +
  ggtitle("Häufigkeit des Überlebensstatus nach Alter")

# Plot anzeigen
print(plot_1)
# 2.) 
# Dataframe zusammenführen
data_2 <- data.frame(fare,survived,age)






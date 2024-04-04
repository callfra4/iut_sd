#Exercice 1 - Importer les données

getwd()
setwd("C:/Users/calli/OneDrive/Documents/R")

#1
df = read.csv(file = "velov.csv", header = TRUE, sep = ";", dec = ",")

#2
summary(df)
class(df$status)
class(df$CodePostal)

#3
df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)

#4
df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK")
tables(df$bornes)
#en réalité, c'est aussi peut-être car la station est fermée OU que des usagers ont déposé leur vélo pile au moment de l'extraction.

#Exercice 2 - L'histogramme

#1
hist(x = df$capacity, main = "Distribution de \n la capacité des stations")

#2
hist(x = df$capacity, main = "Distribution de \n la capacité des stations", breaks = 6)

#3
hist(x = df$capacity, main = "Distribution de \n la capacité des stations", breaks = 6, col = "red")

#4
hist(x = df$capacity, main = "Distribution de \n la capacité des stations", breaks = 6, col = "red", xlab = "Capacity")

#5
abline(h = 100, col = "blue", lty = 2)

#6
hist(x = df$capacity, main = "Distribution de \n la capacité des stations", col = "red", probability = TRUE, xlab = "Capacity")

#7
lines(density(df$capacity), lty = 2, col = "blue", lwd = 4)

#8
hist(x = df$capacity, main = "Distribution de \n la capacité des stations", col = "red", probability = TRUE, xlab = "Capacity", ylim = c(0,0.08))

lines(density(df$capacity), lty = 2, col = "blue", lwd = 2)

#Exercice 3 - Le boxplot

#1
boxplot(x = df$capacity, main = "Boxplot de \n la capacité des stations")

#2
boxplot(x = df$capacity, main = "Boxplot de \n la capacité des stations", horizontal = TRUE)

#3
boxplot(x = df$capacity, main = "Boxplot de \n la capacité des stations", horizontal = FALSE, outline = FALSE)

#4
points(moy, col = "red", pch = 15, cex = 2)

#5
par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes
#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes,main = "Boxplot nb vélos \n 69007", ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, main = "Boxplot nb vélos \n 69008", ylim = c(0,40))
#C'est plus simple d'analyser les deux graphiques si la borne des ordonnées est la même.
# On remarque que la disponibilité des stations est plus homogènes sur le 8ème.

#6
par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,data = df, main = "Dispo vélos vs Stations Bonus")

#7
# Calculer les moyennes de chaque groupe
means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)

#Exercice 4 - Le diagramme

#1
effectif = table(df$bonus)
barplot(height = effectif, main = "Répartition du nombre \n de station bonus")

#2
barplot(height = effectif, main = "Répartition du nombre \n de station bonus", horiz = TRUE)

#3
frequence = prop.table(effectif)
barplot(height = frequence, main = "Répartition en % du nombre \n de station bonus", horiz = TRUE)

#4
effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif, main = "Bonus vs Banking", xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.

#5
#Calcul des pourcentages
frequence = prop.table(x = effectif)
barplot(height = frequence, main = "Bonus vs Banking", xlab = "Station Bonus ?", col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", legend = legend_labels, fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

#6
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence, main = "Bonus vs Banking", xlab = "Station Bonus ?", col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", legend = legend_labels, fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

#7
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence, main = "Bonus vs Banking", xlab = "Station Bonus ?", col = c("red","green"), beside = TRUE)

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", legend = legend_labels, fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

#8
pie(x = effectif, main = "Répartition du nombre \n de station bonus",  col = c("yellow","green"))

#9
etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif, main = "Répartition du nombre \n de station bonus", col = c("yellow","green"), labels = etiquette)

#10
effectif = table(df$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = palette(),
        las = 2)  # Rotation des étiquettes à 90 degrés
#On remarque que les deux premières couleurs se répetent.
print(palette()) # la fonction `palette()` ne dispose que de 8 couleurs

#11
barplot(height = top10, main = "Top 10 sur le \n nombre de station", col = colors(),las = 2)  # Rotation des étiquettes à 90 degrés

print(colors())

#12
dev.print(device = png, file = "export.png", width = 600)

#Exercice 5 - Nuage de points

#1
plot(x = df$stands, y = df$capacity, main = "Place disponible vs Capacité")

#2
plot(x = df$stands, y = df$capacity, main = "Place disponible vs Capacité", xlim = c(0,60), ylim = c(0,60), pch=19)

#3
df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity, main = "Place disponible vs Capacité", xlim = c(0,60), ylim = c(0,60), col = df$bornes, pch=19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes), col = palette(), pch = 19)

#4
myColors <- c("red", "blue", "green")  
# Ajoutez plus de couleurs si nécessaire avec le code HTML des couleurs à la place des noms

# Tracer le graphique
plot(x = df$stands, y = df$capacity, main = "Place disponible vs Capacité", xlim = c(0, 60), ylim = c(0, 60), col = myColors[df$bornes], pch = 19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes), col = myColors, pch = 19)

#5
moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, pch = 15,col = myColors[3],cex = 2)

#Exercice 6 - Cartographie (spoil sur le SD2)

#1 - Executer le code suivant pour créer une carte à partir des colonnes position_longitude et position_latitude
# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte
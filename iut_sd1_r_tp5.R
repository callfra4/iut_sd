#Exercice 1 - Importer les données

#1 - Placer tous les fichiers dans le même dossier nommée nba puis changer le répertoire courant par défaut de votre session RStudio avec la fonction setwd() pour pointer sur ce dossier.
getwd()
setwd("C:/Users/calli/OneDrive/Documents")

#2 - Lister tous les fichiers du dossier nba.
fichiers <- list.files(path = getwd(), pattern = ".csv$", full.names = TRUE)

#3 - Tester la fonction basename() puis file_path_sans_ext() sur le premier fichier de la liste des fichiers afin de retrouver uniquement le nom du fichier sans l'extension .csv.
library(tools)
print(fichiers[1])
nom_fichier_1 <- basename(fichiers[1])
nom_fichier_sans_ext <- file_path_sans_ext(nom_fichier_1)
print(nom_fichier_sans_ext)sc

#4 - Tester la fonction assign() pour importer le jeu de données avec la fonction read.csv(). Qu'est ce qu'on observe dans la fenêtre Environment ?
# Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
assign(x = nom_fichier_sans_ext,  value = read.csv(fichiers[1], sep = ",", dec = "."))
#un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.

#5 - Utiliser ce même procéder dans une boucle for pour importer toutes la liste de fichiers.
# Boucle pour lire chaque fichier CSV
for (fichier in fichiers) {
  # Extraire le nom du fichier sans extension
  nom_objet <- file_path_sans_ext(basename(fichier))
  
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  start_time <- Sys.time()
  assign(nom_objet, read.csv(fichier, sep = ",", dec = "."))
  end_time <- Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}


#Exercice 2 : Les jointures

#1 - Combien de match se sont dérouler à Los Angeles depuis la création de la NBA ?
df_x = subset(team, city == "Los Angeles", select = c("id", "city"))
df_y = subset(game, select = c("game_id", "team_id_home"))
dfJoin = merge(x = df_x, y = df_y, by.x = "id", by.y = "team_id_home", all.x = TRUE)
nrow(dfJoin)
View(dfJoin)

#2 - Quelle est l'affluence moyenne de spectacteur durant ces matchs joués à Los Angeles.
df_x = dfJoin
df_y = subset(game_info, select = c("game_id", "attendance"))
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.x = TRUE)
mean(dfJoin$attendance, na.rm = TRUE)
View(dfJoin)

#3 - Combien d'arbitres différents ont officié durant la saison 2020.
df_x = subset(game_summary, season == 2020,
              select = c("game_id", "season"))
dfJoin = merge(x = df_x, y = officials, 
               by = "game_id",
               all.x = TRUE)
length(unique(dfJoin$official_id))
View(dfJoin)

#4 - Combien de matchs à officié Dick Bavetta par saison ?
df_x = subset(game_summary,
              select = c("game_id", "season"))
df_y = subset(officials, first_name == "Dick" & last_name == "Bavetta")
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.y = TRUE)
View(dfJoin)
table(dfJoin$season)

#Exercice 3 - GOAT : Connexion à une database SQLite

#1 - Installer le package DBI et RSQLite. Puis créer une connexion vers votre fichier nba.sqlite à l'aide de la fonction dbConnect().
library(DBI)
library(RSQLite)
mydb <- dbConnect(SQLite(), "nbaDb.sqlite")

#2 - Lister les tables de la database SQLite à l'aide de la fonction dbListTables().
dbListTables(mydb)

#3 - A l'aide de la fonction dbGetQuery() sélectionner les 5 premières lignes de la table team.
dbGetQuery(mydb, 'SELECT * FROM team LIMIT 5')

#4 - Refaire une des jointures de l'exercice 2 à l'aide de la fonction dbGetQuery().
dfJoin = dbGetQuery(mydb, '....')

#5 - Stocker la table issue de la jointure précédente dans une nouvelle table de la database SQLlite à l'aide de la fonction dbWriteTable(). Vérifier que la table a correctement été créée.
dbWriteTable(mydb, "nom_table", dfJoin)
dbListTables(mydb)

#6 - Fermer la connexion avec la database SQLite.
dbDisconnect(mydb)

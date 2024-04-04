#EXERCICE 1

df <- read.csv(file = "C:/Users/calli/OneDrive/Documents/R/NBA2014_2015.csv", sep = ",", header = TRUE, dec = ".")
# header = TRUE -> pour mettre des noms aux colonnes dans les en-tête et non sur la première ligne 
nrow(df)
ncol(df)
#erreur sur le dataframe 
colnames(df)
df$PERIOD <- as.factor(df$PERIOD)
df$PTS_TYPE <- as.factor(df$PTS_TYPE)
df$SHOOTER = as.factor(df$SHOOTER)
#erreur sur la conversion en as.factor puisque les noms des colonnes n'étaient pas bien écrits comme dans le df
#les pibots proviennent de la console et ne permettent pas d'executer les codes

#EXERCICE 2

length(levels(df$PERIOD))
length(df$PTS_TYPE)
length(df$SHOOTER)
summary(df)
#erreur sur le nom du dataframe
sd(df$SHOT_DIST)
sd(df$SHOT_CLOCK, na.rm = TRUE)
#erreur sur le dataframe + présence de NA
     
     #combien de tirs manqués/réussis
     table(df$SHOT_RESULT)
     #les quartiles
     quantile(df$SHOT_CLOCK, probs = seq(0.25,0.75,0.25), na.rm= TRUE)
     #les déciles
     quantile(df$CLOSE_DEF_DIST, probs = seq(0.1,0.9,0.1))
     #erreur sur le nom de la colonne et sur la formule
     #nombre de matches différents
     liste_game <- unique(df$GAME_ID)
length(liste_game)
#parathèse en + et erreur sur le nom de la colonne
#nombre de joueurs différents
df$SHOOTER <- as.factor(df$SHOOTER)
nlevels(df$SHOOTER)
#erreur sur le nom de la formule "nlevels" et il manquait une parenthèse en fin de formule
       #conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
        df$SHOT_DIST_METRE = (df$SHOT_DIST) * 0.30
       #erreur sur le double
       #nombre de points qu'a rapporté la tentative (0,2 ou 3)  
       df$PTS_MARQUES <- ifelse(df$SHOT_RESULT == "made",yes = df$PTS_TYPE, 0)
       print(df$PTS_MARQUES)
       #On supprime la variable GAME_RESULT car elle n'est pas utile
       df$GAME_RESULT <- NULL
       
       #création d'un objet sans la première colonne GAME_ID
       df2 <- df[ , -1]
       # "-1" se met à droite de la virgule ce qui correspond aux colonnes
      
View(df)

#EXERCICE 3
       

       #Les 100 tirs réussis ou manqués les plus loin
       rang <- order(df$SHOT_DIST, decreasing = FALSE)
       df3 <- df[rang, ]
       df3 <- df[ 1 : 100,  ]
       
       #Les 100 tirs réussis les plus loin
       df4 = subset(df3, SHOT_RESULT == "made")
       df4 <- df[ 1 : 100, ]
       
       #Combien de tirs à 3 points a réussi Kobe Bryant ?
       df_kobe = subset(df,SHOT_RESULT == "made" & PTS_TYPE == 3 & SHOOTER == "kobe bryant")
       
       dim(df_kobe)
       
       #Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
       df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
       rang <- order(df$PTS_MARQUES, decreasing= TRUE)
       df_total_tri <- df_total[rang,]
       df_top5 <-  df_total_tri[c(1:5),  ]
       
#EXERCICE 4
       
       #Des graphiques adaptés selon le type de variable
       
       #construction de la fonction
       build_graph <- function(une_colonne, nom_colonne) {
         if(is.numeric(une_colonne)) {
           print(boxplot(une_colonne, main = nom_colonne))
         }
         else if (as.factor(une_colonne)) {
           tri <- table(une_colonne)
           print(barplot(tri, main = nom_colonne))
         }
         
         #on déroule la fonction sur chaque colonne du data frame.
         
         for (colonne in colnames(df)) {
           build_graph(une_colonne = df[[colonne , ]] , nom_colonne = colone)
         }
       }
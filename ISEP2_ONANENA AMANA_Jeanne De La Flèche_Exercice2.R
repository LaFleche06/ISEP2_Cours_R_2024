#_______Exercice_2_____________________________________________________________________
#créer une base de données de 15 variables et 300 observations
#exporter votre base en fichier csv
#reimporter votre base sur R
#faire des statistiques descriptives sur votre base
#faire manuellemnet le test de Khi2
#______________________________________________________________________________________



library(dplyr)
set.seed(6)
# nombre d'observations
nb_observations <- 300

# Définition des variables

#tirage aléatoire des noms parmi cette liste

nom <- c("Diop", "Ndiaye", "Sow", "Fall", "Diallo", "Gaye", "Cisse", "Thiam", "Sarr", "Niang")
prenom <- c("Fatou", "Mamadou", "Aissatou", "Oumar", "Kadiatou", "Ibrahima", "Khady", "Cheikh", "Aminata", "Modou")
age <- sample(18:65, nb_observations, replace = TRUE)
sexe <- sample(c("Homme", "Femme"), nb_observations, replace = TRUE)
ville <- c("Dakar", "Thiès", "Saint-Louis", "Tambacounda", "Kaolack", "Ziguinchor")
quartier <- c("Medina", "Sicap", "HLM", "Ouakam", "Grand Dakar", "Fass")
region <- c("Dakar", "Thiès", "Saint-Louis", "Tambacounda", "Kaolack", "Ziguinchor")
statut_matrimonial <- sample(c("Célibataire", "Marié(e)", "Divorcé(e)", "Veuf/Veuve"), nb_observations, replace = TRUE)
nombre_enfants <- sample(0:10, nb_observations, replace = TRUE)
profession <- c("Enseignant(e)", "Ingénieur(e)", "Commerçant(e)", "Fonctionnaire", "Artisan(e)", "Étudiant(e)")
nombre_voyages_an <- sample(0:30, nb_observations, replace = TRUE)
niveau_stress <- sample(1:10, nb_observations, replace = TRUE)
etat_sante <- sample(c("Bon", "Mauvais", "Moyen"), nb_observations, replace = TRUE)
heures_travaillees_par_semaine <- sample(20:50, nb_observations, replace = TRUE)
niveau_education <- c("Primaire", "Secondaire", "Supérieur")
revenu_annuel <- sample(200000:1000000, nb_observations, replace = TRUE)

# création de la base de données avec les variables précédentes

data <- data.frame(
  Nom = sample(nom, nb_observations, replace = TRUE),
  Prenom = sample(prenom, nb_observations, replace = TRUE),
  Age = age,
  Sexe = sexe,
  Ville = sample(ville, nb_observations, replace = TRUE),
  Quartier = sample(quartier, nb_observations, replace = TRUE),
  Region = sample(region, nb_observations, replace = TRUE),
  Statut_Matrimonial = statut_matrimonial,
  Nombre_Enfants = nombre_enfants,
  Profession = sample(profession, nb_observations, replace = TRUE),
  Nombre_Voyages_An = nombre_voyages_an,
  Niveau_Stress = niveau_stress,
  Etat_Sante = etat_sante,
  Heures_Travaillees_Semaine = heures_travaillees_par_semaine,
  Niveau_Education = sample(niveau_education, nb_observations, replace = TRUE),
  Revenu_Annuel = revenu_annuel
)

# code pour afficher les premières lignes de la base de données

head(data)

#code pour afficher la base de données

View(data)


# code pour exporter la base de données en fichier CSV

write.csv(data, file = "data.csv", row.names = FALSE)


# code pour reimporter la base de données depuis un fichier CSV

donnees_importees <- read.csv("data.csv")

# Affichage des premières lignes pour vérifier l'importation

head(donnees_importees)

# code pour renommer la base de données

ma_base <- donnees_importees

# Vérifier que la base a été renommée correctement en affichant les premières lignes

head(ma_base)




#après avoir renommé la base, tout ce qu'on fera par la suite devra utiliser le nouveau nom de la base soit le nom "ma_base"



# statistiques descriptives relative à ma base

summary(ma_base)


# Définition des variables dont on veut avoir les tableaux de fréquence

variables <- c("Sexe", "Ville", "Region", "Statut_Matrimonial", "Profession", "Etat_Sante", "Niveau_Education")

# Parcourir chaque variable et générer le tableau de fréquence

for (variable in variables) {
  freq_table <- table(ma_base[[variable]])
  print(paste("Tableau de fréquence pour la variable", variable))
  print(freq_table)
}


#graphiques de certaines variables de ma base de données pour avoir une vue d'ensemble de la repartition de mes données

# Charger les packages nécessaires

library(ggplot2)
library(dplyr)

# Graphique pour l'âge
ggplot(ma_base, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution de l'âge", x = "Âge", y = "Fréquence") +
  theme_minimal()

# Graphique pour le sexe

ggplot(ma_base, aes(x = Sexe)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Répartition par sexe", x = "Sexe", y = "Nombre") +
  theme_minimal()

# Graphique pour la ville

ggplot(ma_base, aes(x = Ville)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Répartition par ville", x = "Ville", y = "Nombre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graphique pour la région 

ggplot(ma_base, aes(x = Region)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Répartition par région", x = "Région", y = "Nombre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graphique pour le statut matrimonial

ggplot(ma_base, aes(x = Statut_Matrimonial)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Répartition par statut matrimonial", x = "Statut Matrimonial", y = "Nombre") +
  theme_minimal()

# Graphique pour l'état de santé

ggplot(ma_base, aes(x = Etat_Sante)) +
  geom_bar(fill = "cyan", color = "black") +
  labs(title = "Répartition par état de santé", x = "État de Santé", y = "Nombre") +
  theme_minimal()

# Graphique pour le niveau d'éducation
ggplot(ma_base, aes(x = Niveau_Education)) +
  geom_bar(fill = "pink", color = "black") +
  labs(title = "Répartition par niveau d'éducation", x = "Niveau d'Éducation", y = "Nombre") +
  theme_minimal()



# pour faire ce tst de Khi2 on doit une fonction nommée Khi2 que nous appliquerons à notre jeu de données pour faire le test


#on définit la fonction du test du Chi2

chi2_test <- function(ma_base, var1, var2) {
  
  # après on crée la tableau de contingence 
  
  contingency_table <- table(ma_base[[var1]], ma_base[[var2]])
  
  # on effectue le test du Chi2
  
  chi2_result <- chisq.test(contingency_table)
  
  # on affiche enfin les résultats. Notons que le test de Khi2 se fait sur des variables catégorielles 
  
  cat("Table de contingence entre", var1, "et", var2, ":\n")
  print(contingency_table)
  cat("\nRésultats du test du Chi2:\n")
  print(chi2_result)
}

# appliquons à présent la fonction précédente à certaines variables caatégorielles de notre jeu de données

chi2_test(ma_base, "Sexe", "Region")

chi2_test(ma_base, "Ville", "Statut_Matrimonial")

chi2_test(ma_base, "Niveau_Education", "Etat_Sante")

chi2_test(ma_base, "Quartier", "Profession")


#Création d'une base de donnée

Data_frame_JENEY <- data.frame(
  Nom = c("Astou","Hasna","Mame","Fatou","Khadidja","Sallah","Absa","Mati","Khadidjatou","Katy"),
  Age = c(26,22,16,21,19,18,33,25,17,20),
  Sexe = c("F","F","M","F","M","F","F","M","F","F"),
  Statut_Matimoniale = c("veuve","Marié","Celibataire","Celibataire","Marié","Marié","Divorcé","Marié","Celibataire","Celibataire"),
  Statut_social = c("Banquière","Journaliste","Maçon","Statisticienne","Infimière","medécin","Comptable","Restauratrice","Statisticienne","Statisticienne"),
  Nb_enfants = c(5,3,0,2,3,0,3,2,1,3)
)
View(Data_frame_JENEY)
Matice1 <- as.matrix(Data_frame_JENEY)

colnames(Matice1)<- c("Name","Age","Sex","marital_status","Social_status","number_of_children")
rownames(Matice1)<- c("SARR","COULIBALY","SOW","NDIAYE","FALL","FAYE","SENE","DIOUF","FAYE","NDIONE")

View(Matice1)

# Statistiques descriptives 
summary(Data_frame_JENEY)

# Supposons que 'df' est votre DataFrame et 'sexe' est la colonne qui contient le sexe
part <- table(Data_frame_JENEY$Sexe)

# Création du diagramme à secteurs
pie(part, labels = names(part), main = "Répartition par sexe")

# Supposons que 'df' est votre DataFrame et 'age' est la colonne qui contient l'âge

breaks <- seq(15, max(Data_frame_JENEY$Age), by = 6)
labels <- paste("[", breaks[-length(breaks)], "-", breaks[-1], "]", sep="")
Data_frame_JENEY$classe_age <- cut(Data_frame_JENEY$Age, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)

# Installation de la bibliothèque ggplot2 si elle n'est pas déjà installée
if (!require(ggplot2)) install.packages('ggplot2')

# Chargement de la bibliothèque ggplot2
library(ggplot2)

# Création du diagramme en barres
ggplot(Data_frame_JENEY, aes(x=classe_age)) +
  geom_bar() +
  xlab("Classe d'âge") +
  ylab("Nombre") +
  ggtitle("Diagramme en barres de la variable 'classe_age'")


#Convexité et Optimisation
#Minimiser f(x, y) = x^4 + y^4 
#sous les contraintes g1(x, y) = x + y - 1 =<0,  x >=0 y >=0 

# Définition de la fonction objectif et des contraintes
f <- function(x, y) {
  return(x^4 + y^4)
}

g1 <- function(x, y) {
  return(x + y - 1)
}

g2 <- function(x, y) {
  return(x)
}

g3 <- function(x, y) {
  return(y)
}

# Définition de la fonction de Lagrange
L <- function(x, y, lambda, mu1, mu2) {
  return(f(x, y) - lambda*g1(x, y) - mu1*g2(x, y) - mu2*g3(x, y))
}

# Dérivées partielles de L
df_dx <- function(x, y, lambda, mu1, mu2) {
  return(4*x^3 - lambda - mu1)
}

df_dy <- function(x, y, lambda, mu1, mu2) {
  return(4*y^3 - lambda - mu2)
}

# Conditions de dualité faible
dual_feasibility <- function(x, y, lambda, mu1, mu2) {
  return(lambda * g1(x, y) == 0 && mu1 * g2(x, y) == 0 && mu2 * g3(x, y) == 0)
}

# Conditions de primal feasibility
primal_feasibility <- function(x, y) {
  return(g1(x, y) <= 0 && g2(x, y) >= 0 && g3(x, y) >= 0)
}

# Initialisation
x <- 0
y <- 0
lambda <- 0
mu1 <- 0
mu2 <- 0

# Taux d'apprentissage
alpha <- 0.01

# Boucle d'optimisation
for (i in 1:1000) {
  # Mise à jour de x et y
  x <- x - alpha * df_dx(x, y, lambda, mu1, mu2)
  y <- y - alpha * df_dy(x, y, lambda, mu1, mu2)
  
  # Mise à jour de lambda, mu1 et mu2
  lambda <- lambda + alpha * g1(x, y)
  mu1 <- mu1 + alpha * g2(x, y)
  mu2 <- mu2 + alpha * g3(x, y)
  
  # Vérification des conditions d'arrêt
  if (primal_feasibility(x, y) && dual_feasibility(x, y, lambda, mu1, mu2) && abs(df_dx(x, y, lambda, mu1, mu2)) < 1e-6 && abs(df_dy(x, y, lambda, mu1, mu2)) < 1e-6) {
    break
  }
}

# Valeur optimale
optimal_value <- f(x, y)
print(paste("La valeur optimale est", optimal_value))



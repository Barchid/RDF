# Chargement des fonctions externes
library ("EBImage")
source ("rdfSegmentation.R")

# Chargement de l'image
nom <- "rdf-chiffre-0-8bits.png"
image <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

###################################### Calculs omega 1 ###########################
nom <- "rdf-chiffre-0-8bits_omega1.png"
omega1 <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h1 <- hist (as.vector (omega1), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# probabilité à priori de omega 1
p_omega1= sum(h1$counts[0:255])/ sum(h$counts[0:255])
#################################################################################



###################################### Calculs omega 2 ###########################
#
nom <- "rdf-chiffre-0-8bits_omega2.png"
omega2 <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h2 <- hist (as.vector (omega2), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# probabilité à priori de omega 2
#### note : le nombre de pixels appartenant à omega deux (Nk) est donné par "sum(h2$counts[0:255])"
p_omega2= sum(h2$counts[0:255])/ sum(h$counts[0:255])
#################################################################################


###### Seuillage automatique ####################################################
#  pour le seuil X calcul de l'erreur d'assignation
somme1 = 0:255
somme2 = 0:255
erreur = 0:255
# recherche du seuil qui donne le minimum en erreur d'assignation
minimum_erreur = 1;
seuil_minimum_erreur = 0;

# POUR CHAQUE X représentant le niveau de gris qui sera le seuil
for (X in 1:255) 
{
  
  #\sum_{\mathbf{X} \in \hat{\omega_1}} P(\mathbf{X}  / \omega_2). P(\omega_2)  
  somme1[X+1]=sum(h1$density[(X+1):256])/sum(h1$density[1:256])
  somme1[X+1]=somme1[X+1]*p_omega1
  
  #\sum_{\mathbf{X} \in \hat{\omega_2}} P(\mathbf{X}  / \omega_2). P(\omega_2)  
  somme2[X+1] = sum(h2$density[1:X])/sum(h2$density[1:256])
  somme2[X+1] = somme2[X+1] * p_omega2
  
  erreur[X+1] = somme1[X+1] + somme2[X+1]
  # seuil corrrespondant Ã  l'erreur minimale
  if (erreur[X+1] < minimum_erreur ) seuil_minimum_erreur = X
  if (erreur[X+1] < minimum_erreur ) minimum_erreur = erreur[X+1]
}

# on replace le seuil (en niveau de gris de 1 à 255) sur une valeur entre 0 et 1 qui sont
# les valeurs qui composent l'histogramme de notre image
seuil = seuil_minimum_erreur/255 

# afficher l'histogramme avec le seuil minimal
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
abline(v = seuil, col = "blue")

binaire_Bayes <- (image - seuil) >= 0
display (binaire_Bayes, "image binaire Bayes", method="raster", all=TRUE)
#################################################################################

############# Binarisation de l'image du chiffre 1 avec seuil de 0 ##############
nom = "rdf-chiffre-1-8bits.png"
image = rdfReadGreyImage(nom)
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
abline(v = seuil, col="blue")
binaire_Bayes <- (image - seuil) >= 0
display (binaire_Bayes, "image binaire Bayes", method="raster", all=TRUE)
#################################################################################


############ Calcul taux d'erreur de classification #############################
# Charger l'image du chiffre à trouver
nom = "rdf-chiffre-1-8bits_classe_a_trouver.png"
image = rdfReadGreyImage(nom)
display (image, "Classe a trouver", method="raster", all=TRUE)

# Soustraire l'image des classes à trouver avec la binarisation
# le 1 - abs(...) sert à faire en sorte que les pixels appartenant à la bonne classe
# apparaissent avec un niveau de gris de 0 dans le diffImg afin de calculer le taux par après
diffImg = 1 - abs(image - binaire_Bayes)
nbins <- 256
hTaux <- hist (as.vector (diffImg), freq=FALSE, breaks = seq (0, 1, 1 / nbins) )

# taux d'erreur = Total des pixels n'appartenant pas à la bonne classe divisé par le total des pixels
tauxErreur = sum(hTaux$counts[2:256])/sum(hTaux$counts[1:256])
display (taux, "Taux d'erreur", method="raster", all=TRUE)
print(tauxErreur)
#################################################################################
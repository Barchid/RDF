# -----------------------------------------------------------------------
# Extraction d'attributs de pixels pour la classification,
# Module RdF, reconnaissance de formes
# Copyleft (C) 2014, Universite Lille 1
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# -----------------------------------------------------------------------

# Chargement des fonctions externes
library ("EBImage")
source ("rdfSegmentation.R")

# Chargement de l'image
#nom <- "rdf-2-classes-texture-0.png"
nom <- "2classes_100_100_8bits_2016.png"
image <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation 0.3
seuil <- 0.35
binaire30 <- (image - seuil) >= 0



# Affichage des deux images
#if (interactive ()) {
#  display (binaire30, "image binaire 0.3")
#  display (binaire35, "image binaire 0.35")
#  display (binaire40, "image binaire 0.4")
#}

###################################### Calculs omega 1 ###########################
#
nom <- "2classes_100_100_8bits_omega1_2016.png"
omega1 <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h1 <- hist (as.vector (omega1), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# probabilité à priori de omega 1
p_omega1= sum(h1$counts[0:255])/ sum(h$counts[0:255])
#################################################################################



###################################### Calculs omega 2 ###########################
#
nom <- "2classes_100_100_8bits_omega2_2016.png"
omega2 <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
nbins <- 256
h2 <- hist (as.vector (omega2), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# probabilité à priori de omega 2
#### note : le nombre de pixels appartenant à omega deux (Nk) est donné par "sum(h2$counts[0:255])"
p_omega2= sum(h2$counts[0:255])/ sum(h$counts[0:255])
#################################################################################


############## nombre de pixels caractérisés par niveau de gris 141 #############
nbPxOmega1 = h1$counts[142]
nbPxOmega2 = h2$counts[142]
nbPxImage = h$counts[142]
#################################################################################

################# Calcul des probabilités conditionnelles #######################
p_141_omega1 = h1$density[142] / sum(h1$counts[0:255]) # Pour omega 1
p_141_omega2 = h2$density[142] / sum(h2$counts[0:255]) # Pour omega 2
p_141_image = h$density[142] / sum(h$counts[0:255]) # Pour l'image
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

# reporter le niveau de gris entre 0 et 1
seuil = seuil_minimum_erreur/255 

# afficher l'histogramme avec le seuil minimal
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
abline(v = seuil, col = "blue")

# binarisation
binaire_Bayes <- (image - seuil) >= 0
display (binaire_Bayes, "image binaire Bayes", method="raster", all=TRUE)
#################################################################################
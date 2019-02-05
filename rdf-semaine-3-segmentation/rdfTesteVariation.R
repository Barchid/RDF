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

# Chargement d'une image
# nom <- "rdf-2-classes-texture-0.png"
# nom <- "rdf-2-classes-texture-1.png"
# nom <- "rdf-2-classes-texture-2.png"
# nom <- "rdf-2-classes-texture-3.png"
nom <- "rdf-2-classes-texture-4.png"
image <- rdfReadGreyImage (nom)
reference = rdfReadGreyImage("rdf-masque-ronds.png")

# Calcul et affichage de son histogramme
nbins <- 64

nivTexture = rdfTextureEcartType(image, 2)
display (nivTexture, "bite", method = "raster", all = TRUE)

h <- hist (nivTexture, breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation
# seuil = 0.52 # texture 0
# seuil = 0.580 # texture 1
# seuil = 0.33 # texture 2
# seuil = 0.45 # texture 3
# seuil = 0 # texture 4

seuilI = readline(prompt = "Entrez le seuil : ")
seuil = as.double(seuilI)
# si on change >= en <=, je change le rôle de blanc et noire
# binaire <- (nivTexture - seuil) >= 0 # si forme claire et fond foncé
binaire <- (nivTexture - seuil) <= 0 # si forme foncé et fond clair

# calculer la differecence par rapport à la reference
difference = reference == binaire
# calculer le pourcentage de difference avec l'image de reference
# length(difference[difference==FALSE]) permet de compter le nombre de valeurs FALSE dans la matrice "difference"
pourcentage = (length(difference[difference==FALSE])) / length(difference) * 100 

print(format(round(pourcentage,2), nsmall = 2))

# Affichage des deux images
if (interactive ()) {
  display (image, nom, method = "raster", all = TRUE)
  display (binaire, "image binaire", method = "raster", all = TRUE)
}

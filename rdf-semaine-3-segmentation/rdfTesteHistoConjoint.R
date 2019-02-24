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
nom <- "rdf-2-classes-texture-1.png"
# nom <- "rdf-2-classes-texture-2.png"
# nom <- "rdf-2-classes-texture-3.png"
# nom <- "rdf-2-classes-texture-4.png"

image <- rdfReadGreyImage (nom)
reference = rdfReadGreyImage("rdf-masque-ronds.png")

# Calcul et affichage de son histogramme

nivTexture = rdfTextureEcartType(image, 2)

display (nivTexture, "niveau de texture", method = "raster", all = TRUE)

his2d = rdfCalculeHistogramme2D(image, 256, nivTexture, 256)
display (his2d, "His2D", method = "raster", all = TRUE)

# traçage du seuil
#lines(x = c(130, 155), y = c(0, 255), col = "red") # texture 0
lines(x = c(130, 155), y = c(0, 255), col = "red") # texture 1
#lines(x = c(130, 155), y = c(0, 255), col = "red") # texture 1
#lines(x = c(130, 155), y = c(0, 255), col = "red") # texture 1
#lines(x = c(130, 155), y = c(0, 255), col = "red") # texture 1

# texture 1
x1 = 130/255
y1 = 0/255
x2 = 155/255
y2 = 255/255


a = -x2 + x1
b = y2 - y1
c = -(a*x1 + b*y1)


for(i in 1:10) {
  for(j in 1:10) {
    for(k in 1:10) {
      a = i/19
      b = j/19
      c =k/19
      
      print(a)
      print(b)
      print(c)
      
      binaire = a * image + b * nivTexture + c < 0
      difference = reference == binaire
      # calculer le pourcentage de difference avec l'image de reference
      # length(difference[difference==FALSE]) permet de compter le nombre de valeurs FALSE dans la matrice "difference"
      pourcentage = (length(difference[difference==FALSE])) / length(difference) * 100 
      if(pourcentage <30){
        
        
        display (binaire, "binarisation", method = "raster", all = TRUE)
        break
      }
    }
  }
}

print("fin")


# élaboration du seuil

#binaire = a * image + b * nivTexture + c < 0

#display (binaire, "binarisation", method = "raster", all = TRUE)
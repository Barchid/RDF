# -----------------------------------------------------------------------
# Extraction d'attributs de forme,
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
source ("rdfMoments.R")

# Chargement de chaque image
noms = c("./point.png",
         "./rdf-carre-10-30deg.png",
         "./rdf-carre-10-45deg.png",
         "./rdf-carre-10.png",
         "./rdf-carre-20.png",
         "./rdf-carre-6.png",
         # "./rdf-chiffre-0.png",
         # "./rdf-chiffre-1.png",
         # "./rdf-chiffre-2.png",
         # "./rdf-chiffre-3.png",
         # "./rdf-chiffre-4.png",
         # "./rdf-chiffre-5.png",
         # "./rdf-chiffre-6.png",
         # "./rdf-chiffre-7.png",
         # "./rdf-chiffre-8.png",
         # "./rdf-chiffre-9.png"
         # "./rdf-patate.png",
         "./rdf-rectangle-diagonal-lisse.png",
         "./rdf-rectangle-diagonal.png",
         "./rdf-rectangle-horizontal.png",
         "./rdf-rectangle-vertical.png"
         # "./rdf-triangle-10-15deg.png",
         # "./rdf-triangle-10-45deg.png",
         # "./rdf-triangle-10-60deg.png",
         # "./rdf-triangle-10.png",
         # "./rdf-triangle-20.png"
        )

for(nom in noms){
  print(paste("Image : ", nom))
  image = rdfReadGreyImage(nom)
  # Calcul du moment centré
  # inertie <- rdfMatriceInertie(image) # transformation du changement de rpeÃ¨te
  # print(inertie)
  # cat("\n")
  
  # print("Matrice d'inertie normalisée")
  # 
  # invertieNormalisee = rdfMatriceInertieNormalise(image)
  # print(invertieNormalisee)
  moment02 = rdfMomentCentreNormalise(image, 0, 2)
  moment20 = rdfMomentCentreNormalise(image, 2, 0)
  moment22 = rdfMomentCentreNormalise(image, 2, 2)

  # cat("\n")
  # print("Hu 1")
  # hu1 = rdfHu1(image)
  # print(hu1)
  # 
  # print("Hu 2")
  # hu2 = rdfHu2(image)
  # print(hu2)
  # 
  # print("Hu 3")
  # hu3 = rdfHu3(image)
  # print(hu3)
  # 
  # print("Hu 4")
  # hu4 = rdfHu4(image)
  # print(hu4)
  # 
  # print("Hu 5")
  # hu5 = rdfHu5(image)
  # print(hu5)
  
  cat("#############################################################\n")
  if (interactive ()) {
    display (image, nom, method = "raster", all = TRUE)
  }
  readline("Next ? > ")
}

nom = "rdf-carre-10-30deg.png";

image <- rdfReadGreyImage (nom)

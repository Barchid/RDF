# -----------------------------------------------------------------------
# Extraction d'attributs de contours,
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
source ("rdfContours.R")

# Chargement d'un contour
#nom <- "rdf-carre-80.txt" # carré
nom <- "rdf-cercle-80.txt" # cercle
cont <- rdfChargeFichierContour (nom)
cont = cont

#img <- rdfReadGreyImage (nom)
#cont = rdfContour(img)


# Afficher le contour
plot (cont, main = "Algorithme de la corde", type = "o", asp = 1, col = "red", ylim = rev (range (Im (cont))))

corde1 = rdfAlgorithmeCorde(cont, 1)
corde05 = rdfAlgorithmeCorde(cont, 0.5)
lines (corde1, main = "Algorithme des cordes à distance maximale de 1", type = "o", asp = 1, col = "blue", ylim = rev (range (Im (corde1))))
lines (corde05, main = "Algorithme des cordes à distance maximale de 0.5", type = "o", asp = 1, col = "black", ylim = rev (range (Im (corde05))))

# cercle bleu
#lines(cont[seq(0, length(cont), by = 4)], col= "blue")

#lines(cont[seq(0, length(cont), by = 8)], col = "green")


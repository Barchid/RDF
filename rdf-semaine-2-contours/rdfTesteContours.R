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
nom <- "rdf-carre-80.txt" # carré
#nom <- "rdf-cercle-80.txt" # cercle
cont <- rdfChargeFichierContour (nom)
cont = cont

# Afficher le contour
plot (cont, main = "Contour", type = "o", asp = 1, col = "red", ylim = rev (range (Im (cont))))

# cercle bleu
#lines(cont[seq(0, length(cont), by = 4)], col= "blue")

#lines(cont[seq(0, length(cont), by = 8)], col = "green")


fourier = fft(cont) / length(cont)

fourier[1] = fourier[1] + complex(real = 4, imaginary = 14)

# afficher reconstitution fourier
inver = fft(fourier, inverse = TRUE) # ca vaut cont
plot (inver, main = "Inversion transformée de Fourier avec ajout d'une constante complexe", type = "o", asp = 1, col = "blue", ylim = rev (range (Im (inver))))
#annule desc fourier
fourierSimplifie = rdfAnnuleDescFourier(fourier, 0.1)
inverSimplifie = fft(fourierSimplifie, inverse = TRUE)
plot (inverSimplifie, main = "Inversion transformée de Fourier simplifiée", type = "o", asp = 1, col = "purple", ylim = rev (range (Im (inverSimplifie))))

#print(fourier)
#print("Inverse")
#print(inver)
#fourier[seq(20,40)] = 0 # on jarte les 20 trucs du milieu

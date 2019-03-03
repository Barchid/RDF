# Chargement des fonctions externes
library ("EBImage")
source ("rdfContours.R")

noms = c(
  "rdf-carre-20.png"
  #"rdf-croix.png"
  #"rdf-patate.png"
  #"rdf-rectangle-horizontal.png"
  #"rdf-triangle-20.png"
)

fourierSimpli = c(
  0.5,
  0.5,
  0.5,
  0.5,
  0.5
)

cordeCoef = c(
  0.5,
  0.5,
  0.5,
  0.5,
  0.5
)

for(nom in noms) {
  image = readImage(nom)
  cont=rdfContour(image)
  
  # Afficher le contour
  plot (cont, main = "Contour", type = "o", asp = 1, col = "red", ylim = rev (range (Im (cont))))  
  
  #Transormée de Fourier
  fourier = fft(cont) / length(cont)
  fourierSimplifie = rdfAnnuleDescFourier(fourier, 0.8)
  inverSimplifie = fft(fourierSimplifie, inverse = TRUE)
  plot (inverSimplifie, main = "Inversion transformée de Fourier simplifiée", type = "o", asp = 1, col = "red", ylim = rev (range (Im (inverSimplifie))))
  
  #Algorithme de la corde
  corde = rdfAlgorithmeCorde(cont, 0)
  
  plot (corde, main = "Algorithme de la corde", type = "o", asp = 1, col = "red", ylim = rev (range (Im (corde))))
}


library ("EBImage")

# Fonction de chargement d'une image en niveaux de gris
rdfReadGreyImage <- function (nom) {
  image <- readImage (nom)
  if (length (dim (image)) == 2) {
    image
  } else {
    channel (image, 'red')
  }
}



################################################################################
# Lecture des deux attributs de l'image étudiée.
################################################################################
reference <- rdfReadGreyImage('rdf-masque-ronds.png') # lecture de l'image de reference pour comparer les résultats
image1 = rdfReadGreyImage("rdf-2-classes-texture-1.png") # attribut 1 => niveaux de gris
image2 = rdfReadGreyImage("rdf-2-classes-texture-1-text.png") # attribut 2 => niveaux de texture
# Afficher les deux attributs 
display (image1, "rdf-2-classes-texture-1.png", method="raster", all=TRUE)
display (image2, "rdf-2-classes-texture-1-text.png", method="raster", all=TRUE)
# Conversion en une matrice des données de taille 16384x2
imagev<-cbind(as.vector(image1), as.vector(image2))
################################################################################



################################################################################
# Affichage des observations de l'image
################################################################################
# Tout afficher en noir car aucune classification
plot(imagev, col="black", xlab = "Niveaux de gris", ylab = "Niveaux de texture", title = "Observations par pixel")
################################################################################



################################################################################
# Application des kmeans sur les données
# (classification non-supervisée en deux classes)
# (on recalcule l'assignation des points et la mise à jour des centres de gravité 30 fois)
################################################################################
ikm<-kmeans(imagev, 2, iter.max = 30)
################################################################################



################################################################################
# Affichage des observations de l'image après classification par kmeans
################################################################################
couleur<- rep("green", length(imagev)) # classe 1 en vert
couleur[ikm$cluster==2]="blue" # Classe 2 en bleu
centers_aff<-cbind(ikm$centers[,1], ikm$centers[,2]) # définition des centres de gravité
plot(imagev, col=couleur, xlab = "Niveaux de gris", ylab = "Niveaux de texture")  # Affichage des points colorés selon la classe
points(centers_aff, col="red", lwd=5) # affichage des centres de gravités en rouge
################################################################################



################################################################################
# Calcul des distances entre les attributs des pixels et les centres de gravités
################################################################################
# distance entre les attributs des pixels et le centre de gravité 1
distance_cdg1 <- (image1-ikm$centers[1,1])**2 + (image2-ikm$centers[2,1])**2
# distance entre les attribut des pixels et le centre de gravité 2
display (distance_cdg1, "distance_cdg1", method="raster", all=TRUE)

#Distance entre les attributs des pixels et le cdg de la classe 2
distance_cdg2 <- (image1-ikm$centers[1,2])**2 + (image2-ikm$centers[2,2])**2
display (distance_cdg2, "distance_cdg2", method="raster", all=TRUE)
################################################################################



################################################################################
# Segmenter l'image en calculant l'image qui résulte du minimum des deux distances
################################################################################

# On affiche en blanc les pixels qui sont plus proches du centre de gravité 2 que du 1
binarisation1 = distance_cdg1 - distance_cdg2 >= 0
display (binarisation1, "Segmentation", method="raster", all=TRUE)
# Calcul du taux de bonne classification via l'image de reference
taux_bonne_classification1 <- 1 - sum(abs(binarisation1 - reference)) / length(binarisation1)

# On affiche en blanc les pixels qui sont plus proches du centre de gravité 1 que du 2
binarisation2 = distance_cdg1 - distance_cdg2 < 0
display (binarisation2, "Segmentation", method="raster", all=TRUE)
# Calcul du taux de bonne classification via l'image de reference
taux_bonne_classification2 <- 1 - sum(abs(binarisation2 - reference)) / length(binarisation2)
################################################################################
library ("EBImage")
# Chargement d'une image en niveaux de gris
rdfReadGreyImage <- function (nom) {
  image <- readImage (nom)
  if (length (dim (image)) == 2) {
    image
  } else {
    channel (image, 'red')
  }
}
image1 = rdfReadGreyImage("rdf-2-classes-texture-1.png")
image2 = rdfReadGreyImage("rdf-2-classes-texture-1-text.png")

display (image1, "rdf-2-classes-texture-1.png", method="raster", all=TRUE)
display (image2, "rdf-2-classes-texture-1-text.png", method="raster", all=TRUE)

 #Conversion en une matrice des données de taille 16384x2
imagev<-cbind(as.vector(image1), as.vector(image2))

# Afficher observations représentant les pixels dans ce plan de projection
plot(imagev, col="black", xlab = "Texture", ylab = "Intensité") # Classe 1 en rouge

#Conversion en une matrice des données de taille 16384x2
ikm<-kmeans(imagev, 2, iter.max = 30)

couleur<- rep("green", length(imagev))
couleur[ikm$cluster==2]="blue"
centers_aff<-cbind(ikm$centers[,1], ikm$centers[,2])
plot(imagev, col=couleur, xlab = "Texture", ylab = "Intensité") 
points(centers_aff, col="red")

#Distance entre les attributs des pixels et le cdg de la classe 1
distance_cdg1 <- (image1-ikm$centers[1,1])**2 + (image2-ikm$centers[2,1])**2
display (distance_cdg1, "distance_cdg1", method="raster", all=TRUE)

#Distance entre les attributs des pixels et le cdg de la classe 2
distance_cdg2 <- (image1-ikm$centers[1,2])**2 + (image2-ikm$centers[2,2])**2
display (distance_cdg2, "distance_cdg2", method="raster", all=TRUE)
display (distance_cdg1 - distance_cdg2 >= 0 , "distance_cdg2", method="raster", all=TRUE)
display (distance_cdg1 - distance_cdg2 <
           0 , "distance_cdg2", method="raster", all=TRUE)

dalton15 = readImage("cas_3_dalton15.png")
#Conversion en une matrice des données de taille 16384x2
imagev<-cbind(as.vector(dalton15[,,1]), as.vector(dalton15[,,2]), as.vector(dalton15[,,3]))
#display (dalton15, "cas_3_dalton15.png", method="raster", all=TRUE)
#Conversion en une matrice des données de taille 16384x2
ikm<-kmeans(imagev, 4, iter.max = 10000)

#Distance entre les attributs des pixels et le cdg de la classe 1
distance_cdg1 <- (dalton15[,,1]-ikm$centers[1,1])**2 + (dalton15[,,2]-ikm$centers[1,2])**2 + (dalton15[,,3]-ikm$centers[1,3])**2
distance_cdg2 <- (dalton15[,,1]-ikm$centers[2,1])**2 + (dalton15[,,2]-ikm$centers[2,2])**2 + (dalton15[,,3]-ikm$centers[2,3])**2
distance_cdg3 <- (dalton15[,,1]-ikm$centers[3,1])**2 + (dalton15[,,2]-ikm$centers[3,2])**2 + (dalton15[,,3]-ikm$centers[3,3])**2
distance_cdg4 <- (dalton15[,,1]-ikm$centers[4,1])**2 + (dalton15[,,2]-ikm$centers[4,2])**2 + (dalton15[,,3]-ikm$centers[4,3])**2

colorMode(distance_cdg1) <- 0
colorMode(distance_cdg2) <- 0
colorMode(distance_cdg3) <- 0
colorMode(distance_cdg4) <- 0

display (distance_cdg1, "distance_cdg1", method="raster", all=TRUE)
display (distance_cdg2, "distance_cdg1", method="raster", all=TRUE)
display (distance_cdg3, "distance_cdg1", method="raster", all=TRUE)
display (distance_cdg4, "distance_cdg1", method="raster", all=TRUE)

display(pmin(distance_cdg1, distance_cdg2, distance_cdg3, distance_cdg4) >= 0.075, "distance_cdg1", method="raster", all=TRUE)

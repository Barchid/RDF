library ("EBImage")
library(rgl) # utilisé pour les plot3d



################################################################################
# Lecture des deux attributs de l'image étudiée.
################################################################################
# Lecture de l'image
dalton15 = readImage("cas_3_dalton15.png")

#Conversion en une matrice des données 82360x3 (pour chaque pixel, on a un attribut Rouge vert et bleu)
imagev<-cbind(as.vector(dalton15[,,1]), as.vector(dalton15[,,2]), as.vector(dalton15[,,3]))

display (dalton15, "cas_3_dalton15.png", method="raster", all=TRUE)

# définition des valeurs des attributs
attribut1 = dalton15[,,1] # attribut de rouge
attribut2 = dalton15[,,2] # attribut de vert
attribut3 = dalton15[,,3] # attribut de bleu
################################################################################



################################################################################
# Affichage des observations de l'image
################################################################################
# Tout afficher en noir car aucune classification
# Affichage 3D de imagev (nécessite RGL)
plot3d(attribut1, attribut2, attribut3,
       type="p",
       xlab="Rouge",
       ylab="Vert",
       zlab="Bleu")
################################################################################



################################################################################
# Application des kmeans sur les données
# (classification non-supervisée en cinq classes)
# (on recalcule l'assignation des points et la mise à jour des centres de gravité 150 fois)
################################################################################
ikm<-kmeans(imagev, 5, iter.max = 150)

# Récupération des centres de graivité
cdg1 = ikm$centers[1,]
cdg2 = ikm$centers[2,]
cdg3 = ikm$centers[3,]
cdg4 = ikm$centers[4,]
cdg5 = ikm$centers[5,]
################################################################################



################################################################################
# Affichage des observations de l'image après classification par kmeans
################################################################################
# Affichage du graphique des pixels suivant leurs attributs avec les couleurs des classes
couleur<- rep("red", length(imagev)) # Classe 1 en rouge
couleur[ikm$cluster==2]="green" # classe 2 en vert
couleur[ikm$cluster==3]="blue" # classe 3 en bleu
couleur[ikm$cluster==4]="orange" # classe 4 en orange
couleur[ikm$cluster==5]="purple" # classe 4 en orange
plot3d(attribut1, attribut2, attribut3, col=couleur, xlab = "Rouge", ylab = "Vert", zlab="Bleu") 

# Afficher les centres de gravité en noir (et en plus gros pour les remarquer)
points3d(ikm$centers, col="black", size= 10)
################################################################################



################################################################################
# Calcul des distances entre les attributs des pixels et les centres de gravités
################################################################################
#Distance entre les attributs des pixels et le centre de gravité de la classe 1
distance_cdg1 <- (attribut1 - cdg1[1])**2 + (attribut2-cdg1[2])**2 + (attribut3-cdg1[3])**2

#Distance entre les attributs des pixels et le centre de gravité de la classe 2
distance_cdg2 <- (attribut1 - cdg2[1])**2 + (attribut2-cdg2[2])**2 + (attribut3-cdg2[3])**2

#Distance entre les attributs des pixels et le centre de gravité de la classe 3
distance_cdg3 <- (attribut1 - cdg3[1])**2 + (attribut2-cdg3[2])**2 + (attribut3-cdg3[3])**2

#Distance entre les attributs des pixels et le centre de gravité de la classe 4
distance_cdg4 <- (attribut1 - cdg4[1])**2 + (attribut2-cdg4[2])**2 + (attribut3-cdg4[3])**2

#Distance entre les attributs des pixels et le centre de gravité de la classe 5
distance_cdg5 <- (attribut1 - cdg5[1])**2 + (attribut2-cdg5[2])**2 + (attribut3-cdg5[3])**2

# mettre le mode de couleur en niveaux de gris pour la binarisation
colorMode(distance_cdg1) <- 0
colorMode(distance_cdg2) <- 0
colorMode(distance_cdg3) <- 0
colorMode(distance_cdg4) <- 0
colorMode(distance_cdg5) <- 0


display (distance_cdg1, "distance_cdg1", method="raster", all=TRUE)
display (distance_cdg2, "distance_cdg2", method="raster", all=TRUE)
display (distance_cdg3, "distance_cdg3", method="raster", all=TRUE)
display (distance_cdg4, "distance_cdg4", method="raster", all=TRUE)
display (distance_cdg5, "distance_cdg5", method="raster", all=TRUE)
################################################################################



################################################################################
# Segmenter l'image en calculant l'image qui résulte du minimum des distances à chaque centre de gravité
################################################################################
# Afficher en blanc les points plus proches du centre de gravité 1 que des autres
display ((distance_cdg1 - distance_cdg2) <= 0 & (distance_cdg1 - distance_cdg3) <= 0 & (distance_cdg1 - distance_cdg4) <= 0 & (distance_cdg1 - distance_cdg5) <= 0, "classe1", method="raster", all=TRUE)

# Afficher en blanc les points plus proches du centre de gravité 2 que des autres
display ((distance_cdg2 - distance_cdg1) <= 0 & (distance_cdg2 - distance_cdg3) <= 0 & (distance_cdg2 - distance_cdg4) <= 0 & (distance_cdg2 - distance_cdg5) <= 0, method="raster", all=TRUE)

# Afficher en blanc les points plus proches du centre de gravité 3 que des autres
display ((distance_cdg3 - distance_cdg1) <= 0 & (distance_cdg3 - distance_cdg2) <= 0 & (distance_cdg3 - distance_cdg4) <= 0 & (distance_cdg3 - distance_cdg5) <= 0, method="raster", all=TRUE)

# Afficher en blanc les points plus proches du centre de gravité 4 que des autres
display ((distance_cdg4 - distance_cdg1) <= 0 & (distance_cdg4 - distance_cdg2) <= 0 & (distance_cdg4 - distance_cdg3) <= 0 & (distance_cdg4 - distance_cdg5) <= 0, "classe4", method="raster", all=TRUE)

# Afficher en blanc les points plus proches du centre de gravité 5 que des autres
display ((distance_cdg5 - distance_cdg1) <= 0 & (distance_cdg5 - distance_cdg2) <= 0 & (distance_cdg5 - distance_cdg3) <= 0 & (distance_cdg5 - distance_cdg4) <= 0, "classe4", method="raster", all=TRUE)
################################################################################
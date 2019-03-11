library("MASS")
library("lattice")

lwd_default = 3 # pour l'affichage

# Chargement des données d'apprentissage et de test
###################################################
load(file = "x_app.data")
load(file = "classe_app.data")
load(file = "x_test.data")
load(file = "classe_test.data")



###################################################
# Afficher les données d'apprentissage
###################################################
plot(x_app[classe_app==1,], col="red", xlab = "Attribut 1", ylab = "Attribut 2" , xlim=c(-12,12), ylim = c(-8, 10), lwd = lwd_default) # Classe 1 en rouge
points(x_app[classe_app==2,], col="green", lwd = lwd_default) # Classe 2 en bleu
points(x_app[classe_app==3,], col="blue", lwd = lwd_default) # Classe 3 en vert
###################################################



###################################################
# ACP sur données d'apprentissage
###################################################
# Calcul de la covariance des données d'apprentissage
covariance_app = cov(x_app)

# Calcul des valeurs et vecteurs propres de covariance
Vp = eigen(covariance_app)

# Vp$vectors[,1] est le vecteur propre principale
# Tracer la droite correspondant au vecteur propre principale
# dont la valeur propre la plus ´elev´ee
pente <- Vp$vectors[2,1]/Vp$vectors[1,1]
abline(a = 0, b = pente, col = "black")


# Projection des points sur l'axe principale
# Calcul du vecteur d'apprentissage projeté sur l'axe principale
ScalarProduct_app_ACP = x_app %*% (Vp$vectors[,1])/sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1]))

XP_app = x_app
# Projection des points
XP_app[,1]= ScalarProduct_app_ACP * Vp$vectors[1,1]
XP_app[,2]= ScalarProduct_app_ACP * Vp$vectors[2,1]
# Affichage des points projet´es
points(XP_app[classe_app==1,], col="red", lwd = lwd_default)
points(XP_app[classe_app==2,], col="green", lwd = lwd_default)
points(XP_app[classe_app==3,], col="blue", lwd = lwd_default)

###################################################



###################################################
# Afficher les données de test
###################################################
plot(x_test[classe_test==1,], col="red", xlab = "Attribut 1", ylab = "Attribut 2" , xlim=c(-12,12), ylim = c(-8, 10), lwd=lwd_default) # Classe 1 en rouge
points(x_test[classe_test==2,], col="green", lwd = lwd_default) # Classe 2 en bleu
points(x_test[classe_test==3,], col="blue", lwd = lwd_default) # Classe 3 en vert
###################################################



###################################################
# ACP sur données de test
###################################################

# Vp$vectors[,1] est le vecteur propre principale
# Tracer la droite correspondant au vecteur propre principale
# dont la valeur propre la plus ´elev´ee
pente <- Vp$vectors[2,1]/Vp$vectors[1,1]
abline(a = 0, b = pente, col = "black")


# Projection des points sur l'axe principale
# Calcul du vecteur d'apprentissage projeté sur l'axe principale
ScalarProduct_test_ACP = x_test %*% (Vp$vectors[,1])/sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1]))

XP_test = x_test
# Projection des points
XP_test[,1]= ScalarProduct_test_ACP * Vp$vectors[1,1]
XP_test[,2]= ScalarProduct_test_ACP * Vp$vectors[2,1]
# Affichage des points projet´es
points(XP_test[classe_test==1,], col="red", lwd = lwd_default)
points(XP_test[classe_test==2,], col="green", lwd = lwd_default)
points(XP_test[classe_test==3,], col="blue", lwd = lwd_default)

###################################################



###################################################
# Appliquer, sur les données de test, la LDA (règle de décision) obtenues grâce aux données d'apprentissage 
###################################################
# Appliquer la LDA sur les données d'apprentissage
x_app_ACP.lda = lda(ScalarProduct_app_ACP,classe_app)

# Appliquer à partir des données d'apprentissage
assigne_test = predict(x_app_ACP.lda, newdata=ScalarProduct_test_ACP)

# Estimation des taux de bonnes classifications
table_classification_test = table(classe_test, assigne_test$class)

# table of correct class vs. classification
print("Bonne classification par classe : ")
print(diag(prop.table(table_classification_test, 1)))

# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))
print("Taux de bonne classification :")
print(taux_bonne_classif_test)

# couleur de la classe 1 LABEL ORIGINAL
couleur<-rep("red",length(x_test)) ;
couleur[classe_test==1]='red'
couleur[classe_test==2]='green'
couleur[classe_test==3]='blue'

# Formes des données (pour le grahique)
shape<-rep(1,length(x_test)) ;
# Forme des données assignées 
shape[assigne_test$class==1]=1 ; # classe 1 = cercle
shape[assigne_test$class==2]=2 ; # Classe 2 = triangle
shape[assigne_test$class==3]=3 ; # Classe 3 = croix

# Affichage des projections apprentissage classées
plot(x_test,col=couleur,pch=shape, xlab = "Attribut 1", ylab = "Attribut 2" , xlim=c(-12,12), ylim = c(-8, 10), lwd = lwd_default)
pente <- Vp$vectors[2,1]/Vp$vectors[1,1]
abline(a = 0, b = pente, col = "black")
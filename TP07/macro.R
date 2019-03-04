library("MASS")
library("lattice")
load(file = "x_app.data")
load(file = "classe_app.data")
load(file = "x_test.data")
load(file = "classe_test.data")

# préparation des données d'apprentissage (les couleurs)
couleur_app<-rep('red',length(x_app))
couleur_app[classe_app==1]='red' # classe 1 en rouge
couleur_app[classe_app==2]='green'  # classe 2 en vert
couleur_app[classe_app==3]='blue' # classe 3 en blue

# préparation des données de test
couleur_test<-rep('red',length(x_app))
couleur_test[classe_test==1]='red'
couleur_test[classe_test==2]='green'
couleur_test[classe_test==3]='blue'

lwd_default = 1# default lwd pour des jolies plots :)

# Affichage données d'apprentissage
plot(x_app, col = couleur_app, lwd = lwd_default, xlim = c(-12, 12), ylim = c(-8, 10))


# Q2
###########################################
cov_app = cov(x_app) # calcul de la covariance des données d'apprentissage
Vp<- eigen(cov_app) # valeurs et vecteurs propres de la covariance des données d'apprentissage

# Affichage de la droite correspondant au vecteur propre
# dont la valeur propre la plus ´elev´ee
pente <- Vp$vectors[2,1]/Vp$vectors[1,1]
abline(a = 0, b = pente, col = "black", lwd=lwd_default)

ScalarProduct_app_ACP <- x_app %*% (Vp$vectors[,1]) / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1]))
x_app_p <-x_app
# Projection des points
x_app_p[,1]= ScalarProduct_app_ACP *Vp$vectors[1,1]
x_app_p[,2]= ScalarProduct_app_ACP *Vp$vectors[2,1]

# Affichage des points projet´es
points(x_app_p[classe_app==1,], col="red") # Projection des points pour la classe 1
points(x_app_p[classe_app==2,], col="green") # Projection des points pour la classe 2
points(x_app_p[classe_app==3,], col="blue") # Projection des points pour la classe 3


# 
# ########################################################################################

# Affichage données de test
plot(x_test, col = couleur_test, lwd = lwd_default, xlim = c(-12, 12), ylim = c(-8, 10))

# Affichage de la droite correspondant au vecteur propre
# dont la valeur propre la plus ´elev´ee
abline(a = 0, b = pente, col = "black", lwd=lwd_default)

ScalarProduct_test_ACP <- x_test %*% (Vp$vectors[,1]) / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1]))
x_test_p <-x_test
# Projection des points
x_test_p[,1]= ScalarProduct_test_ACP *Vp$vectors[1,1]
x_test_p[,2]= ScalarProduct_test_ACP *Vp$vectors[2,1]

# Affichage des points projet´es
points(x_test_p[classe_test==1,], col="red") # Projection des points pour la classe 1
points(x_test_p[classe_test==2,], col="green") # Projection des points pour la classe 2
points(x_test_p[classe_test==3,], col="blue") # Projection des points pour la classe 3


#########################################################################
# Application ALD sur données test
#########################################################################
x_app_p_lda = lda(ScalarProduct_app_ACP,classe_app)
assigne_test<-predict(x_app_p_lda, ScalarProduct_test_ACP)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))


# forme de la classe 1 LABEL ASSIGNATION
# Création du vecteur contenant le code de la forme des données test
shape<-rep(1,length(x_test)) ;
# forme des donn´ees assign´ees `a la classe 2
shape[assigne_test$class==2]=2 ;
# forme des donn´ees assign´ees `a la classe 1
shape[assigne_test$class==1]=1 ;
# forme des donn´ees assign´ees `a la classe 3
shape[assigne_test$class==3]=3 ;

# définition des couleurs de test
couleur = rep('red', length(x_test))
couleur[classe_test==1]='red'
couleur[classe_test==2]='green'
couleur[classe_test==3]='blue'

# Affichage des projections apprentissage class´ees
plot(x_test,col=couleur,pch=shape, lwd = lwd_default,xlab = "X1", ylab = "X2", xlim = c(-12, 12), ylim = c(-8, 10))
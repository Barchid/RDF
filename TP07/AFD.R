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

plot(x_app, col = couleur_app, lwd = lwd_default, xlim = c(-12, 12), ylim = c(-8, 10))

# moyennes des classes (1, 2 et 3)
mean1 <- colMeans(x_app[classe_app==1,])
mean2 <- colMeans(x_app[classe_app==2,])
mean3 <- colMeans(x_app[classe_app==3,])
mean = colMeans(x_app)

# covariances intra-classe
S1 <- cov(x_app[classe_app==1,])
S2 <- cov(x_app[classe_app==2,])
S3 <- cov(x_app[classe_app==3,])

# covariance intra-classe Sw
Sw=S1+S2+S3

# covariance inter-classe
Sb=(mean1-mean)%*%t(mean1-mean)+
    (mean2-mean)%*%t(mean2-mean)+
    (mean3-mean)%*%t(mean3-mean)

# Résolution equation
invSw= solve(Sw) # matrice inverse
invSw_by_Sb= invSw %*% Sb
Vp<- eigen(invSw_by_Sb)

# Affichage de la droite correspondant au vecteur propre
# dont la valeur propre la plus ´elev´ee
pente <- Vp$vectors[2,1]/Vp$vectors[1,1]
abline(a = 0, b = pente, col = "black")

# Projection orthogonale sur le nouvel espace de dimension trouvé grâce au plus grand vecteur propre
ScalarProduct_app_ACP<-x_app
ScalarProduct_app_ACP <- x_app %*% (Vp$vectors[,1]) /sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1]))

XP <-x_app
# Projection des points
XP[,1]= ScalarProduct_app_ACP *Vp$vectors[1,1]
XP[,2]= ScalarProduct_app_ACP *Vp$vectors[2,1]
# Affichage des points projet´es
points(XP[classe_app==1,], col="red")
points(XP[classe_app==2,], col="green")
points(XP[classe_app==3,], col="blue")

########################################################
# Application sur les données tests
########################################################

# Affichage données de test
plot(x_test, col = couleur_test, lwd = lwd_default, xlim = c(-12, 12), ylim = c(-8, 10))

# Affichage de la droite correspondant au vecteur propre
# dont la valeur propre la plus élevée
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

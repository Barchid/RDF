library("MASS")
library("lattice")
load("./simul-2017.Rdata")

#Valeur pour mettre plus en valeur les screens pour le rapport
lwd_default <- 1

##########################################
# Q1.1
##########################################
#Affichage de l'ensemble d'apprentissage
couleur_app<-rep('red',n_app)
couleur_app[classe_app==1]='red'
couleur_app[classe_app==2]='blue'
couleur_app[classe_app==3]='green'

plot(x_app, col = couleur_app, lwd = lwd_default)

#Affichage de l'ensemble de test
couleur_test<-rep('red',n_test)
couleur_test[classe_test==1]='red'
couleur_test[classe_test==2]='blue'
couleur_test[classe_test==3]='green'
plot(x_test, col = couleur_test, lwd = lwd_default)

##########################################
# Q1.2
##########################################
#Affichage de l'ensemble d'apprentissage
plot(x_app, col = couleur_app, lwd = lwd_default)

# Déclaration des vecteurs moyennes
M1 = seq(1,2)
M2 = seq(1,2)
M3 = seq(1,2)

# Vecteur moyenne de la classe 1
M1[1] = mean(x_app[classe_app==1,1])
M1[2] = mean(x_app[classe_app==1,2])

# Vecteur moyenne de la classe 2
M2[1] = mean(x_app[classe_app==2,1])
M2[2] = mean(x_app[classe_app==2,2])

# Vecteur moyenne de la classe 3
M3[1] = mean(x_app[classe_app==3,1])
M3[2] = mean(x_app[classe_app==3,2])

points(M1[1], y = M1[2], pch = 20, col = "red", lwd=15)
points(M2[1], y = M2[2], pch = 20, col = "blue", lwd=15)
points(M3[1], y = M3[2], pch = 20, col = "green", lwd=15)

#Affichage de l'ensemble d'apprentissage
plot(x_app, col = couleur_app, lwd = lwd_default)

# Matrices de co variance
Sigma1 <- matrix(1,2,2)
Sigma2 <- matrix(1,2,2)
Sigma3 <- matrix(1,2,2)

for (i in 1:2) {
  for (j in 1:2) {
    Sigma1[i,j]=cov(as.vector(x_app[classe_app==1,i]), as.vector(x_app[classe_app==1,j]))
    Sigma2[i,j]=cov(as.vector(x_app[classe_app==2,i]), as.vector(x_app[classe_app==2,j]))
    Sigma3[i,j]=cov(as.vector(x_app[classe_app==3,i]), as.vector(x_app[classe_app==3,j]))
  }
}

s1 = s1^2
s2 = s2^2
s3 = s3^2

##########################################
# Q1.3
##########################################

# Grille d'estimation de la densit´e de probabilit´e en 50 intervalles selon 1er attribut
xp1 <- seq(min(x_app[,1]),max(x_app[,1]),length=50)
# Grille d'estimation de la densit´e de probabilit´e en 50 intervalles selon 2eme attribut
xp2 <- seq(min(x_app[,2]),max(x_app[,2]),length=50)

grille <- expand.grid(x1=xp1,x2=xp2)
x_app_lda<-lda(x_app,classe_app)

# Estimation des densit´es de probabilit´es a posteriori dans Zp
grille = cbind(grille[,1],grille[,2])
Zp <- predict(x_app_lda,grille)

#Affichage des lignes de décision
zp_3<-Zp[["posterior"]][,3]-pmax(Zp[["posterior"]][,1],Zp[["posterior"]][,2])
zp_2<-Zp[["posterior"]][,2]-pmax(Zp[["posterior"]][,1],Zp[["posterior"]][,3])

contour(xp1,xp2,matrix(zp_3,50),add=TRUE,levels=0,drawlabels=FALSE, col="green", lwd = lwd_default)
contour(xp1,xp2,matrix(zp_2,50),add=TRUE,levels=0,drawlabels=FALSE, col="blue", lwd = lwd_default)

##########################################
# Q1.4
##########################################

assigne_test <- predict(x_app_lda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test_lda <-table(classe_test, assigne_test[["class"]])
# table of correct class vs. classification
diag(prop.table(table_classification_test_lda, 1))
# total percent correct
taux_bonne_classif_test_lda <-sum(diag(prop.table(table_classification_test_lda)))

# Création du vecteur contenant le code de la forme des données test assignées aux
shape<-rep(1,n_test) ;
# Forme des données assignées 
shape[assigne_test[["class"]]==1]=1 ;
shape[assigne_test[["class"]]==2]=2 ;
shape[assigne_test[["class"]]==3]=3 ;
plot(x_test,col=couleur_test,pch=shape,xlab = "X1", ylab = "X2", lwd = lwd_default)

#Affichage des lignes de décision
contour(xp1,xp2,matrix(zp_3,50),add=TRUE,levels=0,drawlabels=FALSE, col="green", lwd = lwd_default)
contour(xp1,xp2,matrix(zp_2,50),add=TRUE,levels=0,drawlabels=FALSE, col="blue", lwd = lwd_default)

##########################################
# Q1.5
##########################################
#Affichage de l'ensemble d'apprentissage
plot(x_app, col = couleur_app, lwd = lwd_default)

x_app_qda<-qda(x_app,classe_app)

# Estimation des densit´es de probabilit´es a posteriori dans Zp
Zp <- predict(x_app_qda,grille)

#Affichage des lignes de décision
zp_3<-Zp[["posterior"]][,3]-pmax(Zp[["posterior"]][,1],Zp[["posterior"]][,2])
zp_2<-Zp[["posterior"]][,2]-pmax(Zp[["posterior"]][,1],Zp[["posterior"]][,3])
contour(xp1,xp2,matrix(zp_3,50),add=TRUE,levels=0,drawlabels=FALSE, col="green", lwd = lwd_default)
contour(xp1,xp2,matrix(zp_2,50),add=TRUE,levels=0,drawlabels=FALSE, col="blue", lwd = lwd_default)

assigne_test <- predict(x_app_qda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test_qda <-table(classe_test, assigne_test[["class"]])
# table of correct class vs. classification
diag(prop.table(table_classification_test_qda, 1))
# total percent correct
taux_bonne_classif_test_qda <-sum(diag(prop.table(table_classification_test_qda)))

# Création du vecteur contenant le code de la forme des données test assignées aux
shape<-rep(1,n_test) ;
# Forme des données assignées 
shape[assigne_test[["class"]]==1]=1 ;
shape[assigne_test[["class"]]==2]=2 ;
shape[assigne_test[["class"]]==3]=3 ;
# Affichage avec code couleur et forme adapt´ees
plot(x_test,col=couleur_test,pch=shape,xlab = "X1", ylab = "X2", lwd = lwd_default)

#Affichage des lignes de décision
contour(xp1,xp2,matrix(zp_3,50),add=TRUE,levels=0,drawlabels=FALSE, col="green", lwd = lwd_default)
contour(xp1,xp2,matrix(zp_2,50),add=TRUE,levels=0,drawlabels=FALSE, col="blue", lwd = lwd_default)


library("MASS")
library("lattice")
load("./simul-2017.Rdata")

##########################################
# Q1.1
##########################################
couleur<-rep('red',n_app)
couleur[classe_app==1]='red'
couleur[classe_app==2]='blue'
couleur[classe_app==3]='green'
plot(x_app, col = couleur)

# Afficher les probabilités à priori des trois classes
print(p1)
print(p2)
print(p3)

##########################################
# Q1.2
##########################################
# Déclaration des vecteurs moyennes
M1 = seq(1,2)
M2 = seq(1,2)
M3 = seq(1,2)

# Vecteur moyenne de la classe 1
M1[1] = mean(x_app[classe_app==1,1])
M1[2] = mean(x_app[classe_app==1,2])

M2[1] = mean(x_app[classe_app==2,1])
M2[2] = mean(x_app[classe_app==2,2])

M3[1] = mean(x_app[classe_app==3,1])
M3[2] = mean(x_app[classe_app==3,2])

# Matrices de co variance
Sigma1<-matrix(1,2,2)
Sigma2<-matrix(1,2,2)
Sigma3<-matrix(1,2,2)

for (i in 1:2) {
  for (j in 1:2) {
    Sigma1[i,j]=cov(as.vector(x_app[classe_app==1,i]), as.vector(x_app[classe_app==1,j]))
    Sigma2[i,j]=cov(as.vector(x_app[classe_app==2,i]), as.vector(x_app[classe_app==2,j]))
    Sigma3[i,j]=cov(as.vector(x_app[classe_app==3,i]), as.vector(x_app[classe_app==3,j]))
  }
}

# Grille d'estimation de la densit´e de probabilit´e en 50 intervalles selon 1er attribut
xp1 = seq(min(x_app[,1]),max(x_app[,1]),length=50)

# Grille d'estimation de la densit´e de probabilit´e en 50 intervalles selon 2eme attribut
xp2 = seq(min(x_app[,2]),max(x_app[,2]),length=50)

grille<-expand.grid(x1=xp1,x2=xp2) # grille = la grille des combinaisons des valeurs de xp1 et de xp2
x_app.lda<-lda(x_app,classe_app)

# Estimation des densités de probabilités a posteriori dans Zp
grille=cbind(grille[,1],grille[,2])
Zp<-predict(x_app.lda,grille)

# Note : Zp$post[,k] = probabilité à posteriori selon attribut k

# zp = proba à posteriori selon attribut 3 - max(proba à posteriori selon attribut 2, propa à posteriori selon attribut 1)
# formule dans le cours ici :  g3() - max(g2(), g1())
zp1<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp1,50),add=TRUE,levels=0,drawlabels=FALSE, col="red")

################################################################################################
# Manipulation pour afficher les lignes de décisions pour séparer la classe 2 des classes 1 et 3
zp2<-Zp$post[,2]-pmax(Zp$post[,1],Zp$post[,3])
contour(xp1,xp2,matrix(zp2,50),add=TRUE,levels=0,drawlabels=FALSE, col="blue") # afficher ligne des valeurs de zp

################################################################################################

assigne_test<-predict(x_app.lda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))
print(taux_bonne_classif_test)

# Création du vecteur contenant le code de la forme des données test
shape<-rep(1,n_test) ;
# forme des donn´ees assign´ees `a la classe 2
shape[assigne_test$class==2]=2 ;
# forme des donn´ees assign´ees `a la classe 1
shape[assigne_test$class==1]=1 ;
# forme des donn´ees assign´ees `a la classe 3
shape[assigne_test$class==3]=3 ;

# définition des couleurs de test
couleur = rep('red', n_test)
couleur[classe_test==1]='red'
couleur[classe_test==2]='blue'
couleur[classe_test==3]='green'

# Affichage avec code couleur et forme adaptées
plot(x_test,col=couleur,pch=shape,xlab = "X1", ylab = "X2")

# superposer les lignes de décisions 
contour(xp1,xp2,matrix(zp1,50),add=TRUE,levels=0,drawlabels=FALSE, col="blue") # afficher ligne des valeurs de zp
contour(xp1,xp2,matrix(zp2,50),add=TRUE,levels=0,drawlabels=FALSE, col="red") # afficher ligne des valeurs de zp
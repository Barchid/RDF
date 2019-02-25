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
xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
# Grille d'estimation de la densit´e de probabilit´e en 50 intervalles selon 2eme attribut
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)

grille<-expand.grid(x1=xp1,x2=xp2)
x_app.lda<-lda(x_app,classe_app)
# Estimation des densit´es de probabilit´es a posteriori dans Zp
grille=cbind(grille[,1],grille[,2])
Zp<-predict(x_app.lda,grille)
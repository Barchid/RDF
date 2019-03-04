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

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

# Calcul l'entropie de l'ensemble en paramètre
# retourne l'entropie de l'ensemble
entropie = function(etiquettes, ensemble) {
  n = length(ensemble) # taille de l'ensemble
  
  # vecteur du nombre d'exemples de la classe i dans l'ensemble
  classCount = array(rep(0, 40))
  
  # POUR CHAQUE image de l'ensemble
  for(i in 1:n) {
    img = ensemble[i]
    classCount[etiquettes[img]] = classCount[etiquettes[img]]+1
  }
  
  # vecteur des ratios du nombre de chaque représentant d'une classe dans l'ensemble
  classRatio = classCount / n
  
  # Calcul de l'entropie
  entropie = 0
  for(i in 1:40){
    entropie = entropie + (- log2(classRatio[i] ^ classRatio[i]))
    # entropie = entropie + (-log2(classRatio[i] ^ classRatio[i]))
  }
  
  entropie
}

# Sépare l'ensemble des visages en deux ensembles distinctes suivant
# le pixel de coordonnées (x,y)
# retourne les deux ensembles : celui qui contient le pixel (x,y) et celui qui ne contient pas
separation = function(stackedFaces, ensemble, x, y) {
  contient = ensemble[stackedFaces[x,y,] == 1]
  neContientPas = setdiff(ensemble, contient)
  list("contient" = contient, "neContientPas" = neContientPas)
}

# Trouve le pixel qui donne le plus grand gain d'information
# ce pixel trouvé permettra de séparer l'ensemble en deux
partage = function(etiquettes, stackedFaces, ensemble) {
  # Calcul de l'entropie de l'ensemble courant
  H = entropie(etiquettes, ensemble)
  if(is.nan(H)) {
    H = 0
  }
  
  # Création du tableau qui contient, pour chaque pixel, la variation d'entropie équivalente
  deltaH = array(rep(0, 33*40), c(33,40))
  
  # Calcul du deltaH
  # POUR CHAQUE [pixel]
  maxI = 1
  maxJ = 1
  maxDelta = -1
  for(i in 1:33) {
    for(j in 1:40) {
      # séparation de l'ensemble en deux sous-ensembles
      separated = separation(stackedFaces, ensemble, i,j)
      
      # Calcul de l'entropie du sous-ensemble qui contient
      H_A = entropie(etiquettes, separated$contient)
      if(is.nan(H_A)) {
        H_A = 0
      }
      pA = length(separated$contient) / length(ensemble)
      
      # Calcul de l'entropie du sous-ensemble qui ne contient pas le pixel (i,j)
      H_B = entropie(etiquettes, separated$neContientPas)
      if(is.nan(H_B)) {
        H_B = 0 # on jarte ce fils de pute
      }
      pB = length(separated$neContientPas) / length(ensemble)
      
      deltaH[i,j] = H - (pA*H_A) - (pB*H_B)
      if(maxDelta < deltaH[i,j]) {
        maxI = i
        maxJ = j
        maxDelta = deltaH[i,j]
      }
    }
  }
  
  ens = separation(stackedFaces, ensemble, maxI, maxJ)
  if(maxDelta == 0) {
    print("Ensemble")
    print(H)
    # print(maxI)
    # print(maxJ)
    # print("")
    # print(length(ens$contient))
    # print(length(ens$neContientPas))
  }
  list('x' = maxI, 'y' = maxJ, 'contient' = ens$contient, 'neContientPas' = ens$neContientPas)
}

allFaces = rdfReadGreyImage("allFaces.png")

# Création d'un empilement des 400 visages en format de photos 33x40
stackedFaces <- array(rep(0, 33*40*400), c(33, 40, 400))
for (i in 0:19) { for (j in 0:19) {
  stackedFaces[,,(i*20 + j + 1)] = allFaces[(1+j*33) : ((j+1)*33), (1+i*40) : ((i+1)*40)]
}
}



# ON FEY L'1DUKSSION
induction = function(stackedFaces, ensemble, testPicture) {
  # Calcul des classes pour chaque image
  etiquettes = array(rep(0, 400))
  for (i in 1:400) {
    if(i%%10 == 0) {
      etiquettes[i] = i / 10
    }
    else {
      etiquettes[i] = i%/%10 + 1
    }
  }

  while(TRUE) {
    # Vérifier que les images restantes de l'ensemble ne sont pas de la même classe
    isSameClass = TRUE
    tampon = etiquettes[ensemble[1]]
    for(i in 1:length(ensemble)) {
      if(etiquettes[ensemble[i]] != tampon) {
        isSameClass = FALSE
        break
      }
    }
    
    # SI [toutes les images sont de la même classe]
    if(isSameClass) {
      # renvoyer la classe 
      return (tampon)
    }
    
    # Exécuter le partage
    parts = partage(etiquettes, stackedFaces, ensemble)
    
    # SI [je ne peux plus discriminer mon ensemble]
    #     l'arbre décision est arrivé à une feuille, on renvoie les classes trouvées
    if(length(parts$contient) == 0) {
      return (unique(etiquettes[parts$neContientPas]))
    }
    if(length(parts$neContientPas) == 0) {
      return (unique(etiquettes[parts$contient]))
    }
    
    # Trouver l'ensemble (contient ou ne contient pas) où l'image de test existe
    if(testPicture[parts$x,parts$y] == 1) { # SI [testPicture contient le pixel (x,y)]
      ensemble = parts$contient
    } else { # SI [ne contient pas]
      ensemble = parts$neContientPas
    }
  }
}


# ensemble initial (toutes les images au début)
I = seq(1,400)
stackedFacesI = stackedFaces[,,I]

# Affichage de l'image de test
testPicture = as.Image(stackedFaces[,,12])
display(as.Image(testPicture), method = "raster", all = TRUE)

# Démarrage de l'algorithme
resultat = induction(stackedFacesI, I, testPicture)
print(resultat)
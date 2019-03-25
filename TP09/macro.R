numeroFromLettre = function(lettre) { 
  strtoi(charToRaw(lettre),16L)-96 
}

lettreFromNumero = function(numero) {
  rawToChar(as.raw(numero + 96));
}

#Fonction qui retourne l'indice de la lettre associée à la question la plus informative pour
# le sous-ensemble de mots courant, ainsi que la partition du sous-ensemble
# ici, on recherche la lettre qui maximise l'entropie de sorte qu'on arrive à séparer l'ensemble de
# mots "e" en deux sous-ensembles les plus équitables possibles (le mieux serait de trouver des questions
# pour obtenir des ensembles equiprobables)
partage = function(e) {
  n = length(e)
  
  mat = matrix(rep(0,26*n),nrow=26, ncol=n);
  for (i in 1:n)
  {
    c = numeroFromLettre(e[i]);
    mat[c,i] <- 1;
  }
  
  # vecteur des 26 lettres contenant le nombre de mots où la ième lettre de l'alhpabet se trouve
  h = rowSums(mat)
  
  # Entropie de la partition des mots en deux ensembles
  probabilites = h/n # calcul des probabilités
  entropies = - log2(probabilites^probabilites) - log2((1-probabilites) ^ (1 - probabilites))
  
  # trouver la lettre qui sépare le mieux l'ensemble en deux sous-ensemble
  # càd, trouver la lettre qui a la plus grande entropie, donc qui donne la plus grande incertitude
  bestLetter = which.max(entropies)
  
  # trouver le sous-ensemble qui possède la lettre [bestLetter]
  contient = e[mat[bestLetter,] == 1]
  
  # trouver le sous-ensemble qui ne possède pas la lettre [bestLetter]
  neContientPas = setdiff(e, contient)
  
  list("indiceLettre" = bestLetter, "contient" = contient, "neContientPas" = neContientPas)
}

# fonction qui partage un ensemble en deux sous-ensembles
partageIteratif = function(e) {
  sousEnsembles = list()
  sousEnsembles[[1]] = e
  
  # index de parcourt des sous-ensembles à diviser
  i = 1
  
  # TANT QUE [j'ai un ensemble à diviser]
  while(i <= length(sousEnsembles)) {
    aPartager = sousEnsembles[[i]]
    # Si le sous-ensemble à partager n'est pas pur, on peut le partitionner
    if(length(aPartager)>1) {
      parts = partage(aPartager)
      
      e1 = parts$contient
      e2 = parts$neContientPas
      
      if(length(e1) > 0) { 
        #Ajout du premier sous-ensemble (si celui-ci contient quelque chose)
        sousEnsembles[[length(sousEnsembles) + 1]] = e1
      }
      
      if(length(e2) > 0) {
        #Ajout du deuxième sous-ensemble (si celui-ci contient quelque chose)
        sousEnsembles[[length(sousEnsembles) + 1]] = e2
      }
    }
    i = i+1
  }
  
  sousEnsembles
}

jeu = function() {
  # Chargement de la base de noms d'animaux
  source ("rdfAnimaux.txt") # crée un variable "noms"
  
  cat("Wesh bien ou quoi maggle ? Je vais te niquer !")
  
  e = noms
  
  # tant que je ne suis pas sur un noeud pur
  while(length(e) > 1) {
    parts = partage(e)
    
    lettre = lettreFromNumero(parts$indiceLettre)
    
    cat("Ké salope, ton mot, est-ce qu'il a la lettre ", lettre, " ? 1 pour 'Oui', autre pour 'non'")
    answer = scan()
    
    if(answer == 1) {
      e = parts$contient
    }
    else {
      e = parts$neContientPas
    }
  }
  
  cat("J'AI TROUVE FRAIR !!!!")
  
  if(length(e) == 0) {
    cat("Gros, tu m'as couillé, ton mot n'existe pas.")
  }
  else {
    cat("Le mot que tu as, c'est : ", e[1])
  }
}

jeu()

induction = function(e) {
  # SI [ tous les éléments de e sont de la même catégorie]
    # Construire la feuille associée à la classe
  
  # SINON
    # Séparer e en 2 parties selon qui possède la lettre qui divise correctement e 
    # ici, le couple (attribut;test) est attribut = lettre, test = mot contient lettre ?
    # POUR CHAQUE partition
      # induction sur la partition
}
# TP 01 : viol

## Partie 01 : moments en R (viol)

Analysez le code contenu dans ce fichier et expliquez comment est codé le calcul des doubles sommes nécessaires à l'estimation des moments géométriques. Quel est l'intérêt de cette technique?

- Nous disposons de fonctions pour charger des images
- rdfMoment() ==> calculer le moment géométrique où p représente l'ordre de X et q représente l'ordre de Y
	- rdfMoment(im, 1, 0) représente le moment géométrique M10

	- calcul des x à la puissance p, calcul des y à la puissance q et on multiplie 
	- transformer une double somme en un produit de matrices 
- rdfMomentCentre ==> calculer le moment centré (calcul de barycentre) 

- Opérations d'algèbre linéaire : 
	- Somme de ai

- Eigen
	- calculer vecteur propre et valeur propre de forme
	- Si j'ai une valeur qui est nulle, c'est que j'ai un objet qui est un alignement de points
	- Tous les points quin e sont pas sur cet axe d'inertie ne se retrouvent pas sur cette droite
	- Matrice d'inertie 
	- 

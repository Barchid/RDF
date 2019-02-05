# Ex 01 : seuillage fixe 

## Notes du prof 
Oméga 1 et 2, on les connait
On va s'aider de ça pour reclassifier 
Apprentissage supervisé
X chapeau est le seuil 
oméga 1 chapeau est le résultat de l'assignation en dessous du  seuil  (chapeau = après la décision, la classification --> blanc ou noir)

## A quoi correspondent ces valeurs dans l'histogramme ?
Nombre d'occurence d'une intensité (niveau de gris)
On a deux modes et on a choisit plusieurs seuil permettant de déduire nos deux classes, oméga 1 et 2 chapeau

La valeur 0.55 est celle qui classe au mieux les pixels de l'image 

## quelle en est la raison ? 
La marge de densité : c.f. cours 
On essaie de bien calibrer le seuil pour minimiser la somme des surfaces des 
Minimiser la probabilité d'erreur d'assignation 
Equilibrer la marge d'erreur 

# Question 02

P(omega 1) = Nombre de pixels appartenant à classe 1 / Nombre total de pixels

# Question 03
P(omega 1) = 0.57
P(omega 2) = 0.43


# Q3
## Nombre de pixel de niveau de gris 141
omega 1 -> 51
omega 2 -> 67
image total -> 118 (51 + 67)

## Probabilité conditionnelle 
P(X/omega1) = hk(X)/Nk
P(X/I) = 0.00030208 |||||  0.0118
P(X/omega 2) = 0.0009276366 ||||||||| 0.0155814
P(X/omega 1) = 0.0004018467 ||||||||| 0.008947368

# Question 4
Somme1 et 2 ne peuvent pas être mêlé
omega 1 est de X+1 à 256 (délimité par le seuil)
omega 2 est de 1 à X (délimité par le seuil aussi du coup vu qu'on a deux espces)

seuil minimum sera : 0.5450980392 et des poussières
taux d'erreur de classofication otenu qui correspond à la probabilité miniamel d'erreur d'assignation : 0.481


# Question 5
seuil :  0.5568627
mini erreur : 0.80625

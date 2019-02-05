#  pour le seuil X calcul de l'erreur d'assignation
somme1 = 0:255
somme2 = 0:255
erreur = 0:255
# recherche du minimum
minimum_erreur = 1;
seuil_minimum_erreur = 0;

for (X in 1:255) 
{
  # (\sum_{\mathbf{X} \in \hat{\omega_2}} P(\mathbf{X} / \omega_1). P(\omega_1)  
  somme1[X+1]=2*abs(X-127)
  #\sum_{\mathbf{X} \in \hat{\omega_1}} P(\mathbf{X}  / \omega_2). P(\omega_2)  
  somme2[X+1]=3*abs(X-127)
  
  erreur[X+1] = somme1[X+1] + somme2[X+1]
  # seuil corrrespondant à l'erreur minimale
  if (erreur[X+1] < minimum_erreur ) seuil_minimum_erreur = X
  if (erreur[X+1] < minimum_erreur ) minimum_erreur = erreur[X+1]
}

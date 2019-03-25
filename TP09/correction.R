str2int <- function(x) { strtoi(charToRaw(x), 16L) - 96}
int2str <- function(x) { rawToChar(as.raw(x+96))}

partage = function(I) {
  source ("rdfAnimaux.txt") # récupérer les données
  n = length(noms)
  mat = matrix(rep(0,26*n), nrows = 26, ncol = n)
  for(i in 1:n) { mat[str2int(noms[i]), i] = 1 }
  
}
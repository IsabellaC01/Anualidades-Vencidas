
# Valor futuro de una anualidad vencida
valorFuturo=function(A,r,n) {
  VF=(A*((1+r)^n - 1)/r)
  return(VF)
}

# Anualidad dada el valor futuro
anualidad=function(VF,r, n) {
  A=(VF*r/((1+r)^n-1))
  return(A)
}

# Número de pagos dada el valor futuro, tasa y anualidad
plazo=function(VF,A,r) {
  n=log((VF*r/A) + 1)/log(1+r)
  return(n)  
}

# Valor actual de una anualidad vencida
valorActual=function(A,r,n) {
  VA=(A(((1+r)^n)-1))/(r((1+r)^n))
  return(VA)
}

# Anualidad dada el valor actual
anualidadValorActual=function(Va,r,n) {
  A=(r(VA((1+r)^n)))/(((1+r)^t)-1)
  return(A)
}

# Número de pagos dada el valor actual, tasa y anualidad
plazo_por_valor_actual <- function(valor_actual, anualidad, tasa) {
  n=log(anualidad / (anualidad - valor_actual * tasa)) / log(1 + tasa)
  return(ceiling(n))  # Redondear hacia arriba
}

# Tasa del periodo dada el valor actual, plazo y anualidad
tasa_por_valor_actual <- function(valor_actual, anualidad, n) {
  f <- function(tasa) {
    valor_actual - anualidad * (1 - (1 + tasa)^(-n)) / tasa
  }
  return(uniroot(f, c(0, 1))$root)
}


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
plazo_por_valor_futuro <- function(valor_futuro, anualidad, tasa) {
  n <- log((valor_futuro * tasa / anualidad) + 1) / log(1 + tasa)
  return(ceiling(n))  # Redondear hacia arriba
}

# Tasa del periodo dada el valor futuro, plazo y anualidad
tasa_por_valor_futuro <- function(valor_futuro, anualidad, n) {
  f <- function(tasa) {
    valor_futuro - anualidad * ((1 + tasa)^n - 1) / tasa
  }
  return(uniroot(f, c(0, 1))$root)
}

# Valor actual de una anualidad vencida
valor_actual_anualidad <- function(anualidad, tasa, n) {
  return(anualidad * (1 - (1 + tasa)^(-n)) / tasa)
}

# Anualidad dada el valor actual
anualidad_por_valor_actual <- function(valor_actual, tasa, n) {
  return(valor_actual * tasa / (1 - (1 + tasa)^(-n)))
}

# Número de pagos dada el valor actual, tasa y anualidad
plazo_por_valor_actual <- function(valor_actual, anualidad, tasa) {
  n <- log(anualidad / (anualidad - valor_actual * tasa)) / log(1 + tasa)
  return(ceiling(n))  # Redondear hacia arriba
}

# Tasa del periodo dada el valor actual, plazo y anualidad
tasa_por_valor_actual <- function(valor_actual, anualidad, n) {
  f <- function(tasa) {
    valor_actual - anualidad * (1 - (1 + tasa)^(-n)) / tasa
  }
  return(uniroot(f, c(0, 1))$root)
}


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

#Tasa del periodo, conociendo valor futuro, número de pagos y monto de la anualidad.





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
plazoconvalorActual=function(VA,A,r) {
  n=-log(1-((VA*r)/A))/log(1+r)
  return(n) 
}

# Tasa del periodo dada el valor actual, plazo y anualidad
tasaconvalorActual=function(VA,A,n) {
    r= VA-A*(1-(1+r)^(-n))/r
  }
  return(uniroot(f, c(0, 1))$root)
}

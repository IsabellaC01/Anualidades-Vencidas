
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
while (tasa_superior - tasa_inferior > epsilon) {
  VFsupuesto=A*((1+r)^(n)-1)/r
  if (VFsupuesto < VF) {
    tasa_inferior=r  
  } else {
    tasa_superior=r 
  }
  r=(tasa_inferior + tasa_superior)/2 
}
print(r)

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
while (tasa_superior - tasa_inferior > epsilon) {
  VAprimero=A*((1+r)^(n)-1)/r
  if (VAprimero < VF) {
    tasa_inferior=r  
  } else {
    tasa_superior=r 
  }
  r=(tasa_inferior + tasa_superior)/2 
}
print(r)

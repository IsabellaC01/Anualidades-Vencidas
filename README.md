# Anualidades-Vencidas
con este código podemos resolver las funciones para nualidades vencidas

```{r}
source("https://raw.githubusercontent.com/IsabellaC01/Anualidades-Vencidas/refs/heads/main/anualidadesVencidas.R")
```
Se realizan los cálculos:
```{r}
# Creamos objetos con los valores de entrada:
anualidad=1200
tasaPeriodo=0.005
nPeriodos=60
# Calculamos el valor futuro:
valorFuturo=valorFuturo(A=anualidad,r=tasaPeriodo,n=nPeriodos)
# Imprimimos el resultado:
valorFuturo

```

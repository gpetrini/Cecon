function (x, z, y) 
{
    if (x >= z & y == 1) {
        k <- "positiva"
    }
    if (x >= z & y == 2) {
        k <- "positivo"
    }
    if (x < z & y == 1) {
        k <- "negativa"
    }
    if (x < z & y == 2) {
        k <- "negativo"
    }
    return(k)
}

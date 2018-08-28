function (x, digitos = 2, meses = 12) 
{
    k <- 0
    for (i in 0:(meses - 1)) {
        k <- k + x[length(x) - i]
    }
    k <- format(round(k, digits = digitos), big.mark = ".", decimal.mark = ",")
    return(k)
}

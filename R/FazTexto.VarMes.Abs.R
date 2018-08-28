function (x, digitos = 2, meses = 12) 
{
    k <- format(round(x[length(x)] - x[length(x) - (meses - 1)], 
        digits = digitos), big.mark = ".", decimal.mark = ",")
    return(k)
}

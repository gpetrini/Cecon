function (x, digitos = 2) 
{
    k <- format(round(x[length(x)], digits = digitos), big.mark = ".", 
        decimal.mark = ",")
    return(k)
}

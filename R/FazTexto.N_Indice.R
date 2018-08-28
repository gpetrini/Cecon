function (x) 
{
    k <- c(1:length(x))
    k[1] <- (1 + x[1]/100)
    for (i in 2:length(x)) {
        k[i] <- (1 + x[i]/100) * k[i - 1]
    }
    k <- k[length(k)/k]
    return(k)
}

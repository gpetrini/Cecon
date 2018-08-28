function (x) 
{
    k <- c(1:length(x))
    for (i in 0:length(x)) {
        k[i] <- (1 + x[i]/100)^12
    }
    k <- (k - 1) * 100
    return(k)
}

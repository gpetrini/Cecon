function (x, meses = 12) 
{
    m <- c(1:(length(x)))
    for (i in 1:(length(x)(meses - 1))) {
        k <- 1
        for (j in 0:(meses - 1)) {
            k <- k * (1 + x[(i - j) + (meses - 1)]/100)
        }
        k <- (k - 1) * 100
        m[i + (meses - 1)] <- k
    }
    m[1:(meses - 1)] <- NA
    return(m)
}

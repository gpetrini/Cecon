function (x, y, z, w) 
{
    k <- "muito ruim"
    if (x > y) {
        k <- "ruim"
    }
    if (x > z) {
        k <- "bom"
    }
    if (x > w) {
        k <- "muito bom"
    }
    return(k)
}

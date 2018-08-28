###### Cleaning -----
ajuste_xts <- function(dados,
                       col_data = 1,
                       col_dados = 2,
                       remover_NA = FALSE){
    if ("xts" %in% class(dados) 
        | "zoo" %in% class(dados)) {
        message("Série está em xts/zoo, função ajuste_xts não necessária")
        if (sum(is.na(dados)) != 0 & remover_NA == FALSE) {
            message("Séries contém NAs, use remover_NA = TRUE", call. = FALSE)
        }    
        if (remover_NA == FALSE) {
            xts(x = coredata(dados),
                order.by = as.Date(index(dados)))
        } else {
            sem_NA <- !is.na(coredata(dados))
            xts(x = coredata(dados[sem_NA]),
                order.by = as.Date(index(dados[sem_NA])))
        }
    } else if ("ts" %in% class(dados)) {
        dados <- as.xts(dados)
    }
    else {
        if (sum(is.na(dados)) != 0 & remover_NA == FALSE) {
            message("Séries contém NAs, use remover_NA = TRUE", call. = FALSE)
        }
        if (remover_NA == TRUE) {
            teste_NA <- !(is.na(dados[, col_dados]))
            dados_sNA <- dados[teste_NA,]
            proxy <- as.data.frame(dados_sNA)
            proxy_data <- as.Date(proxy[, col_data], origin = proxy[1, col_data])
            xts(x = proxy[, col_dados],
                order.by = proxy_data)
        } else{
            proxy <- as.data.frame(dados)
            proxy_data <- as.Date(proxy[ , col_data], origin = proxy[1,col_data])
            xts(x = proxy[, col_dados],
                order.by = proxy_data)   
        }
    }
}
dput(ajuste_xts, file = "./Area_Comum/Funcoes/ajuste_xts.R") # Exportando Função para arquivo .R

clean_ipeadata <- function(dado_ipea) {
    dado_desl <- unlist(dado_ipea)
    valor <- dado_desl[((length(dado_desl)/2) + 1):length(dado_desl)]
    data_ipea <- dado_desl[1:(length(dado_desl)/2)]
    data_clean <- as.Date(data_ipea)
    xts_name <- deparse((substitute(dado_ipea)))
    xts(x = valor, order.by = data_clean)
}
dput(clean_ipeadata, file = "./Area_Comum/Funcoes/clean_ipeadata.R")

exportar_dados <- function(dados,
                           pasta = "../Tratados", # Se não estiver na mesma pasta, "../Pasta"
                           formato = ".RData") {
    nome_dado <- deparse(substitute(dados)) 
    nome_arquivo <- paste0(nome_dado,formato)
    caminho <- file.path(pasta, nome_arquivo)
    saveRDS(dados,
            file = caminho)
}
dput(exportar_dados, file = "./Area_Comum/Funcoes/exportar_dados.R")

#### Operações -----
media_movel <- function(dados,
                        digitos = 2, 
                        meses = 12,
                        tabela = FALSE) {
    if ("ts" %in% class(dados)) { # Inicia checagem
        dados <- as.xts(dados)
    }
    else if ("tbl_df" %in% class(dados) |
             "tbl" %in% class(dados)) {
        stop("Usar ajuste_xts", call. = FALSE)
    }
    else if ("xts" %in% class(dados) |
             "zoo" %in% class(dados)) {
        dados <- dados
    }
    else  {
        stop("Usar ajuste_xts", call. = FALSE)
    } # Inicia operacao
    y <- c(1:(length(dados)))  #cria variavel y to tamanho da serie inserida
    for (i in 1:(length(dados) - (meses - 1))) { #para i vezes menos meses 
        y[i + (meses - 1)] <- mean(dados[(i + 0):(i + (meses - 1))]) #faz média de meses passos
    }
    y[1:meses] <- NA   #coloca NA nas primeiras 12 entradas
    if (tabela == FALSE) {
        mm_xts <-  xts(x = y,
                       order.by = as.Date(index(dados)))
        return(mm_xts)
    } else {
        mm_xts <-  xts(x = y,
                       order.by = as.Date(index(dados)))
        Tabela_mm <- merge.xts(x = dados,
                               y = mm_xts)
        # serie <- deparse(substitute(dados)) # TODO
        # colnames(Tabela_mm) <- c(as.character(serie), "Media_Movel")
        return(Tabela_mm)
    }
}
dput(media_movel, file = "./Area_Comum/Funcoes/media_movel.R")

##### Taxa acumulada -----
tx_acum <- function(dados,
                        digitos = 2,
                        meses = 12,
                        tabela = FALSE) {
    if ("ts" %in% class(dados)) { # Inicio checagem
            dados <- as.xts(dados)
    }
    else if ("tbl_df" %in% class(dados) |
            "tbl" %in% class(dados)) {
        stop("Usar ajuste_xts", call. = FALSE)
            }
    else if ("xts" %in% class(dados) |
            "zoo" %in% class(dados)) {
        dados <- dados
    }
    else  {
        stop("Usar ajuste_xts", call. = FALSE)
    } # Inicio Operacao
    m <- c(1:(length(dados)))    #cria variavel m to tamanho da serie inserida
    for (i in 1:(length(dados) - (meses - 1))) {#para o total tamanho de x vezes, menos 12
        k <- 1  #cria a var K iterada para chegar na serie m
            for (j in 0:(meses - 1)) { #meses vezes para se taxa anualizada
                k <- k * (1 + dados[i - j + (meses - 1)]/100) #faz multiplica??o de 12 passos para cada posi??o i
                }
            k <- (k - 1)*100#retira 1 finalmente para ficar em valor porcentual
    }
    m[i + (meses - 1)] <- k
    m[1:(meses - 1)] <- NA           #coloca NA nas primeiras 12 entradas
    return(m)
        if (tabela == FALSE) { # Inicio tabela
        acum_xts <-  xts(x = k,
                    order.by = as.Date(index(dados)))
        return(acum_xts)
    } else {
    acum_xts <-  xts(x = k,
                    order.by = as.Date(index(dados)))
    Tabela_accum <- merge.xts(x = dados,
                              y = acum_xts)
    return(Tabela_accum)
    }
}
dput(tx_acum, file = "./Area_Comum/Funcoes/tx_acum.R")

##### Template simples ----
grafico_padrao <- function(dado_xts, 
                           tipo_grafico = geom_line(size = 1), 
                           titulo = NULL,
                           fonte = NULL,
                           x_titulo = NULL, 
                           y_titulo = NULL,
                           tema = theme_classic(), 
                           quebra_data = "1 year",
                           label_data = "%Y",
                           pontos = 0){
    dado_xts <- as.xts(dado_xts)
    ggplot(data = dado_xts,
           aes(
               x = as.Date(index(dado_xts)),
               y = coredata(dado_xts))) +
        tipo_grafico + 
        labs(x = x_titulo,
             y = y_titulo,
             title = titulo,
             caption = paste0("Fonte: ", fonte)) + 
        tema + 
        theme(panel.border = element_blank(), 
              axis.line = element_line(colour = "black", 
                                       size = 0.7), 
              axis.text.x = element_text(angle = 90, 
                                         hjust = 0, 
                                         vjust = 0.5, 
                                         size = 14),
              axis.text.y = element_text(angle = 0, 
                                         hjust = 0, 
                                         vjust = 0.5, 
                                         size = 14),
              text = element_text(size = 10,
                                  family = "TT Times New Roman")) +
        scale_x_date(date_breaks = quebra_data, date_labels = label_data) +
        geom_point(size = pontos)
}
dput(grafico_padrao, file = "./Area_Comum/Funcoes/grafico_padrao.R")

##### Logo cecon -----

add_logo <- function(grafico = last_plot()){
    grafico  %>% 
        ggplotly() %>%  
        layout(images = list(list(source = "http://i.imgur.com/2e3FQaz.png", 
                                  xref = "paper", 
                                  yref = "paper", 
                                  x = 0.02, 
                                  y = 1, 
                                  sizex = 0.25, 
                                  sizey = 0.25, 
                                  opacity = 0.5))) %>%
        config(displayModeBar = TRUE)
}

dput(add_logo, file = "./Area_Comum/Funcoes/add_logo.R")

##### Gráfico cecon -----
grafico_cecon <- function(dado_xts, 
                          logo = TRUE,
                          FUN = grafico_padrao){
    grafico <- dado_xts %>% FUN()
    if (logo == TRUE) {
        grafico_logo <- grafico %>%  add_logo()
        return(grafico_logo)
        
        # if (is.null(transform) == FALSE ) {
        #     grafico_logo %>% add_fun(transform)
        # }
        # 
        # else {
        #     return(grafico_logo)
        # }
        
    } else {
        return(grafico)
    }
}
dput(grafico_cecon, file = "./Area_Comum/Funcoes/grafico_cecon.R")

##### Gráfico rapido

grafico_rapido <- function(dado,
                           remover_NA = FALSE){
    grafico <- dado %>% 
        ajuste_xts(remover_NA = remover_NA) %>% 
        grafico_padrao %>% 
        add_logo
    return(grafico)           }
dput(grafico_rapido, file = "./Area_Comum/Funcoes/grafico_rapido.R")


##### Salvar grafico ----
salvar_grafico <- function(grafico, 
                           pasta = "./Graficos", 
                           formato = ".png"){
    nome_grafico <- deparse(substitute(grafico))
    salvar_nome <- paste0(Sys.Date(),"_",nome_grafico,formato)
    ggsave(file.path(pasta, salvar_nome))
}
dput(salvar_grafico, file = "./Area_Comum/Funcoes/salvar_grafico.R")


#### Fazer texto ----
FazTexto.Cortes1 <- function(x, # Série a ser analisada
                             z, # fronteira
                             y){ # Se feminino, y=1. Se masculino, y=2.
    if (x >= z & y == 1) {k <- "positiva"}
    if (x >= z & y == 2) {k <- "positivo"}
    if (x < z & y == 1) {k <- "negativa"}
    if (x < z & y == 2) {k <- "negativo"}
    return(k)
}
dput(FazTexto.Cortes1, file = "./Area_Comum/Funcoes/FazTexto.Cortes1.R")

FazTexto.Cortes3 <- function(x, # Séria a ser analisada
                             y, # Corte 1
                             z, # Corte 2
                             w # Corte 3
){
    k <- "muito ruim"
    if (x > y) {k <- "ruim"}
    if (x > z) {k <- "bom"}
    if (x > w) {k <- "muito bom"}
    return(k)
}
dput(FazTexto.Cortes3, file = "./Area_Comum/Funcoes/FazTexto.Cortes3.R")

FazTexto.UltimoValor <- function(x, # Série a ser analisada
                                 digitos = 2){
    
    k <- format(round(x[length(x)], 
                      digits = digitos), 
                big.mark = ".", 
                decimal.mark = ",") 
    return(k)
}

dput(FazTexto.UltimoValor, file = "./Area_Comum/Funcoes/FazTexto.UltimoValor.R")

FazTexto.ValorMesAntes <- function(x, # Série a ser analisada
                                   digitos = 2,
                                   meses = 12){
    k <- format(round(x[length(x) -  (meses - 1)],
                      digits = digitos), 
                big.mark = ".", 
                decimal.mark = ",") 
    return(k)
}
dput(FazTexto.ValorMesAntes, file = "./Area_Comum/Funcoes/FazTexto.ValorMesAntes.R")

FazTexto.VarMes.Abs <- function(x, # Série a ser analisada
                                digitos = 2, 
                                meses = 12){
    k <- format(round(x[length(x)] - x[length(x) - (meses - 1)], #subtrai o ultimo valor da série x de meses meses anteriores
                      digits = digitos), 
                big.mark = ".", 
                decimal.mark = ",") 
    return(k)
}

dput(FazTexto.VarMes.Abs, file = "./Area_Comum/Funcoes/FazTexto.VarMes.Abs.R")

FazTexto.VarMes.porc <- function(x, # Série a ser analisada
                                 digitos = 2,
                                 meses = 12){
    k <- format(round((((x[length(x)]/x[length(x) - (meses - 1)]) - 1)*100), #divide o ultimo valor da série x de meses meses anteriores
                      digits = digitos), 
                big.mark = ".", 
                decimal.mark = ",") 
    return(k)
}

dput(FazTexto.VarMes.porc, file = "./Area_Comum/Funcoes/FazTexto.VarMes.porc.R")

FazTexto.TaxaAnualizada <- function(x){#x é a séria a ser análisada
    k <- c(1:length(x)) 
    for (i in 0:length(x)) {         
        k[i] <- (1 + x[i]/100)^12
    }
    k <- (k - 1)*100
    return(k)
}

dput(FazTexto.TaxaAnualizada, file = "./Area_Comum/Funcoes/FazTexto.TaxaAnualizada.R")

FazTexto.AccMes <- function(x,
                            digitos = 2,
                            meses = 12){
    k <- 0
    for (i in 0:(meses - 1)) {         #para i vezes menos meses
        k <- k + x[length(x) - i] #faz soma de 12 passos
    }
    k <- format(round(k, digits = digitos), 
                big.mark = ".", 
                decimal.mark = ",") 
    return(k)
}

dput(FazTexto.AccMes, file = "./Area_Comum/Funcoes/FazTexto.AccMes.R")

FazTexto.TaxaAccMeses <- function(x,
                                  meses = 12){#x ? a s?ria a ser an?lisada
    m <- c(1:(length(x))) #cria variavel m to tamanho da serie inserida
    for (i in 1:(length(x) (meses - 1))) { #para o total tamanho de x vezes, menos 12
        k <- 1  #cria a var K iterada para chegar na seria m
        for (j in 0:(meses - 1)) {#12 vezes para se taxa anualizada 
            k <- k * (1 + x[(i - j) + (meses - 1)]/100) #faz multiplica??o de 12 passos para cada posi??o i
        }
        k <- (k - 1)*100 #retira 1 finalmente para ficar em valor porcentual
        m[i + (meses - 1)] <- k
    }
    m[1:(meses - 1)] <- NA   #coloca NA nas primeiras 12 entradas
    return(m)  #d? como retorno a s?rie taxa anualizada
}

dput(FazTexto.TaxaAccMeses, file = "./Area_Comum/Funcoes/FazTexto.TaxaAccMeses.R")

FazTexto.N_Indice <- function(x){#x é a séria a ser análisada
    k <- c(1:length(x)) #retorna o numero indice para deflacionar para o ultimo valor do ipca
    k[1] <- (1 + x[1]/100)
    for (i in 2:length(x)) {         
        k[i] <- (1 + x[i]/100)*k[i - 1]
    }
    k <- k[length(k)/k]
    return(k)
}

dput(FazTexto.N_Indice, file = "./Area_Comum/Funcoes/FazTexto.N_Indice.R")

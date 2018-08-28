function (grafico = last_plot()) 
{
    grafico %>% ggplotly() %>% layout(images = list(list(source = "http://i.imgur.com/2e3FQaz.png", 
        xref = "paper", yref = "paper", x = 0.02, y = 1, sizex = 0.25, 
        sizey = 0.25, opacity = 0.5))) %>% config(displayModeBar = TRUE)
}

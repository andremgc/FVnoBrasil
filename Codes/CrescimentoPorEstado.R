CrescimentoPorEstado <- ANEELCEP %>% filter (tipo =="UFV" & 
                                                     classe == 'Residencial') %>%
        full_join(expand.grid(data_conexao = seq(ymd("2016/01/01"),
                                                 ymd("2020/01/01"),
                                                 by = "month"), uf = uf)) %>%
        group_by(uf) %>%
        arrange(data_conexao, classe) %>%
        mutate(potencia_instalada_k_w = ifelse(is.na(potencia_instalada_k_w),
                                               0, potencia_instalada_k_w)) %>%
        
        mutate(potencia_acumulada = cumsum(potencia_instalada_k_w)) %>%
        filter(data_conexao>=ymd("2016/01/01") & data_conexao<ymd("2020/01/01") ) %>%
        mutate(data_conexao = ceiling_date(data_conexao, unit="month"))%>%
        group_by(data_conexao, uf) %>%
        summarise(potencia_acumulada = max(potencia_acumulada)) %>%
        mutate (uf=fct_reorder(uf,potencia_acumulada, .fun = min, .desc = T ),
                potencia_acumulada=potencia_acumulada/1000)

pCrescimento <-
        ggplot(CrescimentoPorEstado,aes(x=data_conexao, y=potencia_acumulada)) +
        geom_line(aes(color=uf)) +
        labs (color = "UF", x= 'Ano', y='Potencia instalada acumulada [MW]')+
        theme_classic()+
        scale_y_continuous(expand = c(0.0,0)) + 
        scale_x_date(expand = c(0,0))
ggplotly(pCrescimento)

pCrescimentoStack <- 
        ggplot(CrescimentoPorEstado,aes(x=data_conexao, y=potencia_acumulada)) +
        geom_area(aes(fill=uf)) +
        labs (fill = "UF", x= 'Ano', y='Potencia instalada acumulada [MW]') +
        theme_classic() +
        scale_y_continuous(expand = c(0.0,0)) + 
        scale_x_date(expand = c(0,0))
pCrescimentoStack
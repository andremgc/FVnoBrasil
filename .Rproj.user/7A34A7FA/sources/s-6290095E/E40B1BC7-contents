var = 'uf'
var2= quo(uf)

CrescimentoPorEstado <- ANEELCEP %>% 
        filter (tipo =="UFV" & classe == 'Residencial') %>%
        full_join(
           expand.grid(data_conexao = seq(ymd("2016/01/01"),
                                                 ymd("2020/01/01"),
                                                 by = "month"), 
                              unique(ANEELCEP[[var]])) %>%
                   rename (!!var := Var2)
                            ) %>%
        group_by_at(vars(var)) %>%
        arrange(data_conexao, classe) %>%
        mutate(potencia_instalada_k_w = ifelse(is.na(potencia_instalada_k_w),
                                               0, potencia_instalada_k_w)) %>%
        
        mutate(potencia_acumulada = cumsum(potencia_instalada_k_w)) %>%
        filter(data_conexao>=ymd("2016/01/01") & data_conexao<ymd("2020/01/01") ) %>%
        mutate(data_conexao = ceiling_date(data_conexao, unit="month"))%>%
        ungroup() %>%
        group_by_at(vars(data_conexao,var)) %>%
        summarise(potencia_acumulada = max(potencia_acumulada)) %>%
        mutate (potencia_acumulada=potencia_acumulada/1000)

CrescimentoPorEstado[[var]]=fct_reorder(CrescimentoPorEstado[[var]],
                                        CrescimentoPorEstado$potencia_acumulada,
                                        .fun = max, .desc = T )
               
pCrescimento <-
        ggplot(CrescimentoPorEstado,aes(x=data_conexao, y=potencia_acumulada)) +
        geom_line(aes(color=!!var2)) +
        labs (color = "UF", x= 'Ano', y='Potencia instalada acumulada [MW]')+
        theme_classic()+
        scale_y_continuous(expand = c(0.0,0)) + 
        scale_x_date(expand = c(0,0))
ggplotly(pCrescimento)

pCrescimentoStack <- 
        ggplot(CrescimentoPorEstado,aes(x=data_conexao, y=potencia_acumulada)) +
        geom_area(aes(fill=!!var2), position = "fill") +
        labs (fill = "UF", x= 'Ano', y='Potencia instalada acumulada [MW]') +
        theme_classic() +
        scale_y_continuous(expand = c(0.0,0)) + 
        scale_x_date(expand = c(0,0))
ggplotly(pCrescimentoStack)

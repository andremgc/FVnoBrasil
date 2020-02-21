# Define funcao para resolver possiveis problemas com os nomes
OperaNomes <- function(Nomes) { 
        
        Nomes <- str_to_upper(Nomes) %>%
                iconv ( from="UTF-8",to="ASCII//TRANSLIT") %>%
                # Remove espacos sobrando 
                str_replace_all(" +", " ") %>%
                str_replace_all("^ +", "") %>%
                str_replace_all(" +$", "") 
        # Tansforma A'As em A
        
        Nomes <- Nomes %>% str_replace_all("([A-Z])\'\\1", "\\1")
        
        
        # Remove D's; etc. Se houver barra de espaçõ em seguida, tembem retira.
        Nomes <- Nomes %>% str_replace_all("([A-Z])\' ?", "\\1")
        # Remove pontos no fim.
        
        # VER CASOS
        
        Nomes <- Nomes %>% str_replace_all(" DO ", " ")
        Nomes <- Nomes %>% str_replace_all(" DOS ", " ")
        Nomes <- Nomes %>% str_replace_all(" DE ", " ")
        Nomes <- Nomes %>% str_replace_all(" DAS ", " ")
        Nomes <- Nomes %>% str_replace_all(" DA ", " ")
        
        
        Nomes <- Nomes %>% str_replace_all("\\.$", "")
        
        # Transforma pontos no meio em barra de espaço
        Nomes <- Nomes %>% str_replace_all("([A-Z])(\\.)([A-Z])", "\\1 \\3")
        # Letras sozinhas com ponto em letra sozinha apenas
        Nomes <- Nomes %>% str_replace_all("( |^)([A-Z])(\\.)( |$)",
                                           "\\1\\2\\4")
        # Transformar D AGOSTINHO etc. para DAGOSTINHO
        Nomes <- Nomes %>% str_replace_all(" D ([AEIOU])", " D\\1")
        
        # Remove Todas os elementos nao  alfabeticos ou espaco
        Nomes <- Nomes %>% str_replace_all("[^a-zA-Z0-9, ]"," ") %>%
                # Remove espacos sobrando 
                str_replace_all(" +", " ") %>%
                str_replace_all("^ +", "") %>%
                str_replace_all(" +$", "") 
        
        Nomes <- Nomes %>% str_replace_all("MOGI", "MOJI")
        Nomes <- Nomes %>% str_replace_all("LUIZ", "LUIS")
        Nomes <- Nomes %>% str_replace_all("^EMBU$", "EMBU ARTES")
        Nomes <- Nomes %>% str_replace_all("^PIUI$", "PIUMHI")
        Nomes <- Nomes %>% str_replace_all("^PICARRAS$", "BALNEARIO PICARRAS")
        Nomes <- Nomes %>% str_replace_all("DOESTE", "OESTE")
        Nomes <- Nomes %>% str_replace_all("BATAIPORA", "BATAYPORA")
        Nomes <- Nomes %>% str_replace_all("ITAPAGE", "ITAPAJE")
        Nomes <- Nomes %>% str_replace_all("PARATI", "PARATY")
        Nomes <- Nomes %>% str_replace_all("CAITANO", "CAETANO")
        Nomes <- Nomes %>% str_replace_all("^ITAMARACA$", "ILHA ITAMARACA")
        Nomes <- Nomes %>% str_replace_all("^BRASOPOLIS$", "BRAZOPOLIS")
        Nomes <- Nomes %>% str_replace_all("^DONA EUSEBIA$", "DONA EUZEBIA")
        Nomes <- Nomes %>% str_replace_all("^IGUARACI$", "IGUARACY")
        Nomes <- Nomes %>% str_replace_all("^BRODOSKI$", "BRODOWSKI")
        Nomes <- Nomes %>% str_replace_all("^CHIAPETA$", "CHIAPETTA")
        Nomes <- Nomes %>% str_replace_all("^IPAUCU$", "IPAUSSU")
        Nomes <- Nomes %>% str_replace_all("^PEDRO REGIO$", "PEDRO REGIS")
        Nomes <- Nomes %>% str_replace_all("^QUELUZITA$", "QUELIZITO")
        Nomes <- Nomes %>% str_replace_all("DILERMANO AGUIAR$", "DILERMANDO AGUIAR")
        
        # Vale a pena rodar uma função ate este ponto e ver casos remanescentes.
        Nomes <- Nomes %>% str_to_title()
        

}

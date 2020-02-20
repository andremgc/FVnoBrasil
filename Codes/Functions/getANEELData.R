getANEELData <- function(pagina = 1L, acao = "buscar", login = "",
                           nom_pessoa = "", id_agente = "", dat_conexao_inicio = "",
                           dat_conexao_fim = "", RetornaNumeroPaginas = F) {
        
        # Carregando Pacotes Necessários
        
        suppressPackageStartupMessages({
                require("httr", quietly = TRUE, warn.conflicts = FALSE)
                require("xml2", quietly = TRUE, warn.conflicts = FALSE)
                require("rvest", quietly = TRUE, warn.conflicts = FALSE)
                require("janitor", quietly = TRUE, warn.conflicts = FALSE)
                require("stringi", quietly = TRUE, warn.conflicts = FALSE)
                require("dplyr", quietly = TRUE, warn.conflicts = FALSE)
        })
        
        # get the page like a browser
        
        httr::GET(
                url = "http://www2.aneel.gov.br/scg/gd/VerGD.asp",
                query = list(
                        pagina = as.character(as.integer(pagina)),
                        acao = acao,
                        login = login,
                        NomPessoa = nom_pessoa, 
                        IdAgente = id_agente,
                        DatConexaoInicio = dat_conexao_inicio,
                        DatConexaoFim = dat_conexao_fim
                )
        ) -> res
        
        httr::stop_for_status(res)
        
        # DON'T PARSE IT YET
        
        out <- httr::content(res, as = "text")
        
        # Remove beginning & trailing whitespace from lines
        
        l <- stri_trim_both(stri_split_lines(out)[[1]])
        
        # Now, remove all form-component lines and all blank lines
        
        l[-c(
                which(grepl("<form", l, fixed = TRUE)), 
                which(grepl("<input", l, fixed = TRUE)),
                which(l == "")
        )] -> l
        
        if (RetornaNumeroPaginas == T){
                # The number of pages is equal to the ceilling of the number of
                # the number of generators devided by 1000. 
                generators <- as.numeric(gsub("\\.","",l[81]))
                pages <- ceiling(generators/1000)
        }
        
        # Get the indices of all the <td> tags that should have a <tr> before them but dont
        
        to_fix <- c()
        for (i in 1:(length(l)-1)) {
                if (all(c(
                        grepl("/tr", l[i]), grepl("td", l[i+1])
                ))) {
                        to_fix <- c(to_fix, (i+1))
                }
                
        }
        
        # Fix them
        
        l[to_fix] <- sprintf("<tr>%s", l[to_fix])
        
        # NOW WE CAN PARSE IT
        
        x <- read_html(paste0(l, collapse="\n"))
        
        # Find the table in a less breakable way
        
        tabl <- html_nodes(x, xpath=".//table[@class = 'tabelaMaior']/tr/td[contains(., 'UNIDADES')]/../..")
        
        # Remove the useless title row that makes html_table() cry
        
        xml_remove(html_node(tabl, xpath=".//tr[1]"))
        
        # Remove the bottom pagination row that makes html_table() cry
        
        xml_remove(html_node(tabl, xpath=".//tr/td[@colspan = '20']/.."))
    
    # Extract the table with better column names
    
    xdat <- html_table(tabl, header=TRUE, trim=TRUE)[[1]] 
    xdat <- janitor::clean_names(xdat)
    xdat <- dplyr::tbl_df(xdat)
    
    if (RetornaNumeroPaginas == T){
            output <- list (xdat, pages)
    } else{
            return(xdat)
    }
    
}

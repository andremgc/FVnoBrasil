source("./Codes/Functions/getANEELData.R")

iteraSiteANEEL<-function(filename = NULL){
        leitura <- getANEELData(RetornaNumeroPaginas = T)
        
        dfProsumers <- leitura[[1]]
        Npages <- leitura[[2]]
        
        pb <- progress_bar$new(total = Npages-1, clear = FALSE)
        library(progress)
        pb$tick(0)
        # iteração para todas as páginas do site
        for (i in 2:Npages){
                
                # leitura do site. Pode dar erro devido a conexões, então, 
                # tenta-se 3 vezes.
                attempt <- 1
                dfAux <- NULL
                while( is.null(dfAux) && attempt <= 3 ) {
                        attempt <- attempt + 1
                        try(
                                dfAux <- getANEELData(pagina = i)
                        )
                } 
                if (attempt >3) {
                        warning(paste0("Erro na leitura da pagina ", i))
                }
                dfProsumers<-rbind(dfAux,dfProsumers)
                pb$tick()
        }
        
        dfProsumers$data_conexao <- lubridate::dmy(dfProsumers$data_conexao)
        
        if(is.null(filename)){
        filename <- paste0('geradoresANEEL_',
                           as.character(lubridate::today()),
                           '.RDS')
        }
        saveRDS(dfProsumers, paste0 ("./Data/",filename))
        
}



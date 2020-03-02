source("./Codes/Functions/OperaNomes.R")

## Leitura de dados
ANEELCEP <- readRDS( "./Data/ANEELCEP.RDS") %>% 
        mutate(potencia_instalada_k_w =
                       str_replace_all(potencia_instalada_k_w,"\\.","") %>%
                       str_replace_all(",",".")  %>%
                       as.numeric()) %>%
        mutate(municipio_padronizado = OperaNomes(municipio))

## Filtra cidades de interesse e geração FV residencial para os quais há endereço
GDFVResidencialCidadesInteresse <- ANEELCEP %>%
        filter(tipo =="UFV" & classe == 'Residencial' &
                       municipio %in% c("Rio de Janeiro", "Uberlândia",
                                        "Belo Horizonte"
                                        ) &
                       !is.na(Endereço) ) %>%
        # Retira acentos dos nomes de municipio e une endereço e municipio
        mutate(municipio = municipio %>%
                       iconv ( from="UTF-8",to="ASCII//TRANSLIT"), 
                enderecoCompleto = 
                       paste(Endereço, municipio, sep = ", ")) %>%
        # Retira explicações sobre o endereço que atrapalham na busca
        mutate(enderecoCompleto = enderecoCompleto  %>% 
                       str_replace_all(" - .*,"," , ") %>% 
                       str_replace_all("\\(.*,"," ,"))

Enderecos <- unique(GDFVResidencialCidadesInteresse$enderecoCompleto)

## Leitura de informações dado endereços

# Primeira linha
Leitura1 <- osm_search(Enderecos[1],
                key="YX3mSlZacHzWUEqCo2TdtiYUG2hsCq05",
                email="andmgdc@gmail.com")

LeituraNA <- Leitura1 %>% mutate_all(function(x) NA)

Local <- cbind(Endereco = Enderecos[1], Leitura1)
Locais <- Local

# Adiciona-se uma linha por iteração
for (i in 2:length(Enderecos)){
        Leitura <- osm_search(Enderecos[i]  ,
                            key="YX3mSlZacHzWUEqCo2TdtiYUG2hsCq05",
                            email="andmgdc@gmail.com")
        
        # Se houver erro na busca, tentar repetir sem caracteres especiais
        if (nrow(Leitura) == 0){
                Sys.sleep(1+runif(1))
                Enderecos[i] = Enderecos[i]%>%
                        iconv ( from="UTF-8",to="ASCII//TRANSLIT") 
                Leitura <- osm_search(Enderecos[i]  ,
                                      key="YX3mSlZacHzWUEqCo2TdtiYUG2hsCq05",
                                      email="andmgdc@gmail.com")
                # Se ainda houver erro na leitura, considerar como NA
                if (nrow(Leitura) == 0){
                        Leitura <- LeituraNA
                }
        }
        
        # Algumas leituras apresentam a coluna aidicional 'icon'. 
        # Se houver tal coluna adicionar, removê-la.
        if (ncol(Leitura) == 15){
                Leitura <- Leitura %>% select(-icon)
        }
        
        # Fazer com que a linha adicionada contenha o endereço usado na busca
        Local <- cbind(Endereco = Enderecos[i], Leitura)
        
        # Adicionar a linha ao df principal
        Locais <- rbind(Locais,Local)
        
        # Fazer uma pausa, visando respeitar o uso dos dados, que estão em 
        # servidores fornecidos voluntariamente
        Sys.sleep(2)
        
        print(i)
}

saveRDS(LocaisF, file="./Data/LatLong.RDS")
               
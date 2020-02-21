library(dplyr)
library(readr)
library(stringr)

# Lendo Dados dos Geradores
ANEEL <- readRDS("./Dados/geradoresANEEL_2020-02-18.RDS" )%>%
        rename(CEP = cep) %>%
        mutate(CEP = str_replace_all(CEP,"-",""))

# Lendo Dados dos 3 bancos de CEP
dataCEP <- read_tsv("./Dados/ceps.txt", col_names = F) %>%
        rename(CEP = X1,
               Cidade = X2,
               Bairro = X3,
               Endereço = X4)
dataCEP2 <- read_tsv("./Dados/utfcepos.txt", col_names = F) %>%
        rename(CEP = X1,
               Estado = X2,
               Cidade = X3,
               Bairro = X4,
               Endereço = X5) %>% 
        select(CEP, Cidade, Bairro, Endereço)
dataCEP3 <- read_tsv("./Dados/vrceps-UTF8.txt", col_names = F) %>%
        rename(CEP = X1,
               Estado = X2,
               Cidade = X3,
               Bairro = X4,
               Endereço = X5) %>% 
        select(CEP, Cidade, Bairro, Endereço)

# Quais CEPS há em dataCEP2 mas não em dataCEP
dataCEP2extra <- anti_join(dataCEP2,dataCEP, by ="CEP")

# Adicionando estes CEPS a dataCEP
dataCEP <- rbind(dataCEP1, dataCEP2extra)

# Quais CEPS há em dataCEP3 mas não em dataCEP1
dataCEP3extra <- anti_join(dataCEP3,dataCEP, by ="CEP")

# Adicionando estes CEPS a dataCEP
dataCEP <- rbind(dataCEP, dataCEP3extra)

# Unindo os dados
ANEELCEP <- ANEEL %>% left_join(dataCEP, by = "CEP")

# Salvando df.
saveRDS(ANEELCEP, "./Data/ANEELCEP.RDS")
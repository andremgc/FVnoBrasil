uf <- c("AC","AL","AM","AP","BA","CE","DF","ES",
        "GO","MA","MG","MS","MT","PA","PB","PE",
        "PI","PR","RJ","RN","RO","RR","RS","SC",
        "SE","SP","TO")

SS <-    c("SE/CE", "NE" ,"N", "N", "NE","NE","SE/CE","SE/CE",
           "SE/CE","NE","SE/CE","SE/CE","SE/CE","N","NE","NE",
           "NE","S","SE/CE","NE","SE/CE","N","S","S",
           "NE","SE/CE","N")

Subsistemas<-data.frame(uf,SS)

saveRDS(Subsistemas, "./Data/Subsistemas.RDS")

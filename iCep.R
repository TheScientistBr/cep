library(data.table)
library("tidyr")
library("stringr")
require("sqldf")

files <- list.files(path = "correios",pattern = "LOG_LOGRADOURO*")
bairro <- read.delim("correios/LOG_BAIRRO.TXT", header=F, stringsAsFactors=FALSE,sep = "@")
names(bairro) <- c("BAI_NU","UFE_SG","LOC_NU","BAI_NO","BAI_NO_ABREV")
ceps <- data.frame()
for(file in files) {
        cep <- read.delim(paste0("correios/",file), header=FALSE, stringsAsFactors=FALSE,sep = "@")
        ceps <- rbind(ceps,as.data.frame(cep))
}
names(ceps) <- c("LOG_NU","UFE_SG","LOC_NU","BAI_NU_INI","BAI_NU_FIM","LOG_NO","LOG_COMPLEMENTO",
                "CEP","TLO_TX","LOG_STA_TLO","LOG_NO_ABREV")
head(ceps)
remove(cep,file,files)

conEnd <- function(myQuery) {
        myQuery <- paste0("select * from ceps where LOG_NO LIKE ","'",myQuery,"'")
        ender <- sqldf(myQuery)
        bairr <- conBairro(ender$BAI_NU_INI)
        response <- data.frame(endereco=paste(ender$TLO_TX, ender$LOG_NO), bairro=bairr$BAI_NO, uf=ender$UFE_SG, cep=ender$CEP)
        return(response)
}

conBairro <- function(myBairro) {
        myBairro <- paste0("select * from bairro where BAI_NU LIKE ","'",myBairro,"'")
        response <- sqldf(myBairro)
}

conEnd("joaquim lírio")






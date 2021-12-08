library(readxl)
library(tidyr)
library("tidyverse")

## Banco de Dados
futevolei <- read_excel("ftv.xlsx")

## Total de Jogos mapeados
unique(futevolei$Jogo)

## Total de Atletas mapeados
unique(futevolei$Atleta)

##Totais de Ataque
futevolei$diagonalcurta <- (futevolei$DCC + futevolei$DCS + futevolei$DCE)
futevolei$pingomeio <- (futevolei$PMC + futevolei$PMS + futevolei$DCE)
futevolei$pingoatras <- (futevolei$PAC + futevolei$PAS + futevolei$PAE)
futevolei$diagonalmeio <- (futevolei$DMC + futevolei$DMS + futevolei$DME)
futevolei$meio <- (futevolei$MMC + futevolei$MMS + futevolei$MME)
futevolei$paralelacurta <- (futevolei$PCC + futevolei$PCS +futevolei$PCE)
futevolei$diagonallonga <- (futevolei$DLC + futevolei$DLS + futevolei$DLE)
futevolei$meiofundo <- (futevolei$MFC + futevolei$MFS + futevolei$MFE)
futevolei$paralela <- (futevolei$PLC + futevolei$PLS + futevolei$PLE)

##Percentual de Acerto de Ataque
futevolei$pontoDC <- (futevolei$DCC * 100) / futevolei$diagonalcurta
futevolei$pontoPM <- (futevolei$PMC * 100) / futevolei$pingomeio
futevolei$pontoPA <- (futevolei$PAC * 100) / futevolei$pingoatras
futevolei$pontoDM <- (futevolei$DMC * 100) / futevolei$diagonalmeio
futevolei$pontoM <- (futevolei$MMC * 100) / futevolei$meio
futevolei$pontoPC <- (futevolei$PCC * 100) / futevolei$paralelacurta
futevolei$pontoDL <- (futevolei$DLC * 100) / futevolei$diagonallonga
futevolei$pontoMF <- (futevolei$MFC * 100) / futevolei$meiofundo
futevolei$pontoPL <- (futevolei$PLC * 100) / futevolei$paralela

## Organizar o Banco de Dados com as novas variáveis
futevolei <- futevolei %>% select(Jogo, Atleta, Nivel, diagonalcurta, pontoDC, pingomeio, pontoPM, pingoatras,
                      pontoPA, diagonalmeio, pontoDM, meio, pontoM, paralelacurta, pontoPC, 
                      diagonallonga, pontoDL, meiofundo, pontoMF, paralela, pontoPL, 
                                    everything())

## Médias de Volume por Tipo de Ataque
medias <- c(mean(futevolei$diagonalcurta), mean(futevolei$pingomeio), mean(futevolei$pingoatras), 
            mean(futevolei$diagonalmeio), mean(futevolei$meio), mean(futevolei$paralelacurta), 
            mean(futevolei$diagonallonga), mean(futevolei$meiofundo), mean(futevolei$paralela))

##Acerto por Tipo de Ataque / Geral
acerto <- c((sum(futevolei$DCC) * 100) / sum(futevolei$diagonalcurta), (sum(futevolei$PMC) * 100) / sum(futevolei$pingomeio), 
            (sum(futevolei$PAC) * 100) / sum(futevolei$pingoatras), (sum(futevolei$DMC) * 100) / sum(futevolei$diagonalmeio), 
            (sum(futevolei$MMC) * 100) / sum(futevolei$meio), (sum(futevolei$PCC) * 100) / sum(futevolei$paralelacurta), 
            (sum(futevolei$DLC) * 100) / sum(futevolei$diagonallonga), (sum(futevolei$MFC) * 100) / sum(futevolei$meiofundo), 
            (sum(futevolei$PLC) * 100) / sum(futevolei$paralela))

## Novo Banco de Dados - Totais de ataque x Acerto de ataque
futevolei2 <- data.frame(x = medias, y = acerto)

futevolei2 <- futevolei2 %>% rename(Médias = x, Acerto = y)

futevolei2$Ataques <- c("Diagonal Curta", "Pingo de Meio", "Pingo Atrás",
                  "Diagonal Média", "Meio", "Paralela Curta", 
                  "Diagonal Longa", "Meio Fundo", "Paralela") 

futevolei2 <- futevolei2 %>% select(Ataques, Acerto, Médias)

futevolei3 <- futevolei %>% group_by(Nivel) %>% 
  filter(pontoDC > mean(pontoDC, na.rm=TRUE)) %>% 
  ungroup () %>% droplevels(.)

futevolei$Jogo[which(futevolei$Atleta == "Sonda")]

sonda <- futevolei[which(futevolei$Atleta == "Sonda" & futevolei$Jogo == "2"), byrow = TRUE, 2:11]

futevolei$Jogo[which(futevolei$pingomeio == 8)]

futevolei$total_ataque <- futevolei$diagonalcurta + futevolei$pingomeio + futevolei$pingoatras + 
                futevolei$diagonalmeio + futevolei$meio + futevolei$paralelacurta + 
                  futevolei$diagonallonga + futevolei$meiofundo + futevolei$paralela

futevolei$total_acerto <- futevolei$DCC + futevolei$DLC + futevolei$DMC + 
                          futevolei$MMC + futevolei$MFC + futevolei$PAC + 
                          futevolei$PCC + futevolei$PLC + futevolei$PMC

futevolei$total_percentual <- (futevolei$total_acerto * 100) / futevolei$total_ataque

futevolei4 <- data.frame(futevolei$Atleta, futevolei$Nivel, futevolei$total_ataque, 
                         futevolei$total_acerto, futevolei$total_percentual)

futevolei4 <- futevolei4 %>% rename(Atleta = 1, Nivel = 2, Total = 3, Acerto = 4, Percentual = 5)

futevolei <- futevolei %>% select(Jogo, Atleta, Nivel, total_ataque, total_acerto, total_percentual, diagonalcurta, pontoDC, pingomeio, pontoPM, pingoatras,
                                  pontoPA, diagonalmeio, pontoDM, meio, pontoM, paralelacurta, pontoPC, 
                                  diagonallonga, pontoDL, meiofundo, pontoMF, paralela, pontoPL, 
                                  everything())

#library

library(purrr)
library(dplyr)
library(readr)

#liste

oxyplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/PLACEBO_git",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

listdfoxyplacebo <- lapply(oxyplacebo , r)
listdfoxyplacebo <- lapply(listdfoxyplacebo , function (bob){bob[,-8]})
listdfoxyplacebo <- lapply(listdfoxyplacebo , na.omit)
names(listdfoxyplacebo) <- tools::file_path_sans_ext(basename(oxyplacebo))


listsmO2avgplacebo <- lapply(listdfoxyplacebo, function(bob){as.data.frame(bob[,4])})
coln <- "smO2avg"
listsmO2avgplacebo <- lapply(listsmO2avgplacebo, setNames, coln)

# ajout des NA pour chaque sujet

nb_lignes <- t(as.data.frame(lapply(listsmO2avgplacebo, nrow)))
nb_lignes <- as.data.frame(nb_lignes)
nb_lignes_max <- 10000
dfnbligne <- mutate(nb_lignes, lignes_manquantes = nb_lignes_max - V1)
dfnbligne <- dfnbligne[,-1]
dfnbligne <- as.data.frame(dfnbligne)

for(i in 1:nrow(dfnbligne)){nadf[[i]] <- rep(0 , times = dfnbligne[i, ]) %>% list()}

list_dfsmO2avgplacebo <- list()

for(i in 1:length(listsmO2avgplacebo)) {
  element1 <- listsmO2avgplacebo[[i]]
  element2 <- nadf[[i]]
  fusion <- rbind(element1 , element2)
  list_dfsmO2avgplacebo[[i]] <- fusion
}

names(list_dfsmO2avgplacebo) <- tools::file_path_sans_ext(basename(oxyplacebo))

#export csv de la smO2avg pour traitement dans matlab

for (i in 1:length(list_dfsmO2avgplacebo)) {
  write.csv(list_dfsmO2avgplacebo[[i]],
            file = paste0(names(list_dfsmO2avgplacebo)[i], ".csv"),
            row.names = F)
}


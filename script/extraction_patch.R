#library

library(purrr)
library(dplyr)
library(readr)
library(plyr)


#list


oxypatch <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/PATCH_git",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

r <- function (bob) {read.csv(bob, sep = "," , skip = 3)}

listdfoxypatch <- lapply(oxypatch , r)
listdfoxypatch <- lapply(listdfoxypatch , function (bob){bob[,-8]})
listdfoxypatch <- lapply(listdfoxypatch , na.omit)
names(listdfoxypatch) <- tools::file_path_sans_ext(basename(oxypatch))

colnames(listdfoxypatch$RemiFalconPOST48A) <-
  c("mm.dd",
    "hh.mm.ss",
    "SmO2.Live",
    "SmO2.Averaged",
    "THb",
    "Lap",
    "Session.Ct")
colnames(listdfoxypatch$SimonLopezPOST48A) <-
  c("mm.dd",
    "hh.mm.ss",
    "SmO2.Live",
    "SmO2.Averaged",
    "THb",
    "Lap",
    "Session.Ct")
colnames(listdfoxypatch$BriceAlmuniaPOSTA) <-
  c("mm.dd",
    "hh.mm.ss",
    "SmO2.Live",
    "SmO2.Averaged",
    "THb",
    "Lap",
    "Session.Ct")

listsmO2avgpatch <- lapply(listdfoxypatch, function(bob){as.data.frame(bob[,4])})
coln <- "smO2avg"
listsmO2avgpatch <- lapply(listsmO2avgpatch, setNames, coln)

# ajout des NA pour chaque sujet

nb_lignes <- t(as.data.frame(lapply(listsmO2avgpatch, nrow)))
nb_lignes <- as.data.frame(nb_lignes)
nb_lignes_max <- 10000
dfnbligne <- mutate(nb_lignes, lignes_manquantes = nb_lignes_max - V1)
dfnbligne <- dfnbligne[,-1]
dfnbligne <- as.data.frame(dfnbligne)

for(i in 1:nrow(dfnbligne)){nadf[[i]] <- rep(0 , times = dfnbligne[i, ]) %>% list()}

list_dfsmO2avgpatch <- list()

for(i in 1:length(listsmO2avgpatch)) {
  element1 <- listsmO2avgpatch[[i]]
  element2 <- nadf[[i]]
  fusion <- rbind(element1 , element2)
  list_dfsmO2avgpatch[[i]] <- fusion
}

names(list_dfsmO2avgpatch) <- tools::file_path_sans_ext(basename(oxypatch))

#export csv de la smO2avg pour traitement dans matlab

for (i in 1:length(list_dfsmO2avgpatch)) {
  write.csv(list_dfsmO2avgpatch[[i]],
            file = paste0(names(list_dfsmO2avgpatch)[i], ".csv"),
            row.names = F)
}


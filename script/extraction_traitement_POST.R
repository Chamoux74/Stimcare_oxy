#library

library(purrr)
library(dplyr)
library(readr)
library(plyr)
library(ggplot2)
library(ggpubr)


#list patch

postoxypatch <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/POST/PATCH",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

r <- function (bob) {read.csv(bob, sep = "," , skip = 3)}

listdfpostoxypatch <- lapply(postoxypatch , r)
listdfpostoxypatch <- lapply(listdfpostoxypatch , function (bob){bob[,-8]})
listdfpostoxypatch <- lapply(listdfpostoxypatch , na.omit)
names(listdfpostoxypatch) <- tools::file_path_sans_ext(basename(postoxypatch))

#list / importation placebo

postoxyplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/POST/placebo",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

listdfpostoxyplacebo <- lapply(postoxyplacebo , r)
listdfpostoxyplacebo <- lapply(listdfpostoxyplacebo , function (bob){bob[,-8]})
listdfpostoxyplacebo <- lapply(listdfpostoxyplacebo , na.omit)
names(listdfpostoxyplacebo) <- tools::file_path_sans_ext(basename(postoxyplacebo))

# selection smO2 live

listdfpostoxyplacebo <- lapply(listdfpostoxyplacebo , function (bob){bob[,3] %>% as.data.frame()})
listdfpostoxypatch <- lapply(listdfpostoxypatch , function (bob){bob[,3] %>% as.data.frame()})

nom <- "smO2live"

listdfpostoxyplacebo <- lapply(listdfpostoxyplacebo , setNames, nom)
listdfpostoxypatch <- lapply(listdfpostoxypatch , setNames , nom)

#moyenne de la 1 à la 8'30

dfpostoxyplacebo <-
  as.data.frame(lapply(listdfpostoxyplacebo , function(bob) {
    mean(bob[120:1020, ])
  }))

dfpostoxypatch <-
  as.data.frame(lapply(listdfpostoxypatch , function(bob) {
    mean(bob[120:1020,])
  }))

dfpostoxypatch <- as.data.frame(t(dfpostoxypatch))
dfpostoxyplacebo <- as.data.frame(t(dfpostoxyplacebo))

patch <- rep("patch", time = 11)
placebo <- rep("placebo" , time = 11)

sujet <- c(1, 2, 3, 4, 5,6, 7 , 8, 9 , 10, 11)

instant <- rep("post", time = 11)

dfpostoxypatch <- cbind(dfpostoxypatch, patch, sujet, instant)
colnames(dfpostoxypatch) <- c("smO2live" , "condition", "sujet" , "timing")

dfpostoxyplacebo <- cbind(dfpostoxyplacebo, placebo, sujet, instant)
colnames(dfpostoxyplacebo) <- c("smO2live" , "condition", "sujet" , "timing")

dfpostfinal <- rbind(dfpostoxypatch, dfpostoxyplacebo)

dfpostfinal %>% ggplot(aes(x= condition , y = smO2live)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_line(aes(group = sujet) , linetype = "dotted") +
  geom_boxplot(aes(fill = condition), alpha = 0.3) +
  geom_point(aes(color = factor(sujet))) +
  stat_summary(aes(group = condition),
               fun.y = mean ,
               shape = 23  ,
               fill = "black" ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")) +
  stat_compare_means(method = "wilcox.test", paired = TRUE , label.x = 1.5) +
  labs(color = "Sujet")


#moyenne de 7'30 à 8'30


dfpostoxyplaceboce <-
  as.data.frame(lapply(listdfpostoxyplacebo , function(bob) {
    mean(bob[900:1020, ])
  }))

dfpostoxypatchce <-
  as.data.frame(lapply(listdfpostoxypatch , function(bob) {
    mean(bob[900:1020,])
  }))

dfpostoxypatchce <- as.data.frame(t(dfpostoxypatchce))
dfpostoxyplaceboce <- as.data.frame(t(dfpostoxyplaceboce))

dfpostoxypatchce <- cbind(dfpostoxypatchce, patch, sujet, instant)
colnames(dfpostoxypatchce) <- c("smO2live" , "condition", "sujet" , "timing")

dfpostoxyplaceboce <- cbind(dfpostoxyplaceboce, placebo, sujet, instant)
colnames(dfpostoxyplaceboce) <- c("smO2live" , "condition", "sujet" , "timing")

dfpostfinalce <- rbind(dfpostoxypatchce, dfpostoxyplaceboce)

#library

library(purrr)
library(dplyr)
library(readr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(reticulate)


#list patch

preoxypatch <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/PRE/PATCH",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

r <- function (bob) {read.csv(bob, sep = "," , skip = 3)}

listdfpreoxypatch <- lapply(preoxypatch , r)
listdfpreoxypatch <- lapply(listdfpreoxypatch , function (bob){bob[,-8]})
listdfpreoxypatch <- lapply(listdfpreoxypatch , na.omit)
names(listdfpreoxypatch) <- tools::file_path_sans_ext(basename(preoxypatch))

#list / importation placebo

preoxyplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/PRE/placebo",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

listdfpreoxyplacebo <- lapply(preoxyplacebo , r)
listdfpreoxyplacebo <- lapply(listdfpreoxyplacebo , function (bob){bob[,-8]})
listdfpreoxyplacebo <- lapply(listdfpreoxyplacebo , na.omit)
names(listdfpreoxyplacebo) <- tools::file_path_sans_ext(basename(preoxyplacebo))

# selection smO2 live

listdfpreoxyplacebo <- lapply(listdfpreoxyplacebo , function (bob){bob[,3] %>% as.data.frame()})
listdfpreoxypatch <- lapply(listdfpreoxypatch , function (bob){bob[,3] %>% as.data.frame()})

nom <- "smO2live"

listdfpreoxyplacebo <- lapply(listdfpreoxyplacebo , setNames, nom)
listdfpreoxypatch <- lapply(listdfpreoxypatch , setNames , nom)

#moyenne de la 1 à la 8'30

dfpreoxyplacebo <-
  as.data.frame(lapply(listdfpreoxyplacebo , function(bob) {
    mean(bob[120:1020, ])
  }))

dfpreoxypatch <-
  as.data.frame(lapply(listdfpreoxypatch , function(bob) {
    mean(bob[120:1020,])
  }))

dfpreoxypatch <- as.data.frame(t(dfpreoxypatch))
dfpreoxyplacebo <- as.data.frame(t(dfpreoxyplacebo))

patch <- rep("patch", time = 11)
placebo <- rep("placebo" , time = 11)

sujet <- c(1, 2, 3, 4, 5,6, 7 , 8, 9 , 10, 11)

instant <- rep("PRE", time = 11)

dfpreoxypatch <- cbind(dfpreoxypatch, patch, sujet, instant)
colnames(dfpreoxypatch) <- c("smO2live" , "condition", "sujet" , "timing")

dfpreoxyplacebo <- cbind(dfpreoxyplacebo, placebo, sujet, instant)
colnames(dfpreoxyplacebo) <- c("smO2live" , "condition", "sujet" , "timing")

dfprefinal <- rbind(dfpreoxypatch, dfpreoxyplacebo)

dfprefinal %>% ggplot(aes(x= condition , y = smO2live)) +
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


dfpreoxyplaceboce <-
  as.data.frame(lapply(listdfpreoxyplacebo , function(bob) {
    mean(bob[900:1020, ])
  }))

dfpreoxypatchce <-
  as.data.frame(lapply(listdfpreoxypatch , function(bob) {
    mean(bob[900:1020,])
  }))

dfpreoxypatchce <- as.data.frame(t(dfpreoxypatchce))
dfpreoxyplaceboce <- as.data.frame(t(dfpreoxyplaceboce))

dfpreoxypatchce <- cbind(dfpreoxypatchce, patch, sujet, instant)
colnames(dfpreoxypatchce) <- c("smO2live" , "condition", "sujet" , "timing")

dfpreoxyplaceboce <- cbind(dfpreoxyplaceboce, placebo, sujet, instant)
colnames(dfpreoxyplaceboce) <- c("smO2live" , "condition", "sujet" , "timing")

dfprefinalce <- rbind(dfpreoxypatchce, dfpreoxyplaceboce)

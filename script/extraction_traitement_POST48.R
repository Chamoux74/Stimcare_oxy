#library

library(purrr)
library(dplyr)
library(readr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(rstatix)


#list patch

post48oxypatch <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/POST48/PATCH",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

r <- function (bob) {read.csv(bob, sep = "," , skip = 3)}

listdfpost48oxypatch <- lapply(post48oxypatch , r)
listdfpost48oxypatch <- lapply(listdfpost48oxypatch , function (bob){bob[,-8]})
listdfpost48oxypatch <- lapply(listdfpost48oxypatch , na.omit)
names(listdfpost48oxypatch) <- tools::file_path_sans_ext(basename(post48oxypatch))

#list / importation placebo

post48oxyplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/OXY_EFFORT/data/POST48/placebo",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

listdfpost48oxyplacebo <- lapply(post48oxyplacebo , r)
listdfpost48oxyplacebo <- lapply(listdfpost48oxyplacebo , function (bob){bob[,-8]})
listdfpost48oxyplacebo <- lapply(listdfpost48oxyplacebo , na.omit)
names(listdfpost48oxyplacebo) <- tools::file_path_sans_ext(basename(post48oxyplacebo))

# selection smO2 live

listdfpost48oxyplacebo <- lapply(listdfpost48oxyplacebo , function (bob){bob[,3] %>% as.data.frame()})
listdfpost48oxypatch <- lapply(listdfpost48oxypatch , function (bob){bob[,3] %>% as.data.frame()})

nom <- "smO2live"

listdfpost48oxyplacebo <- lapply(listdfpost48oxyplacebo , setNames, nom)
listdfpost48oxypatch <- lapply(listdfpost48oxypatch , setNames , nom)

#moyenne de la 1 à la 8'30

dfpost48oxyplacebo <-
  as.data.frame(lapply(listdfpost48oxyplacebo , function(bob) {
    mean(bob[120:1020, ])
  }))

dfpost48oxypatch <-
  as.data.frame(lapply(listdfpost48oxypatch , function(bob) {
    mean(bob[120:1020,])
  }))

dfpost48oxypatch <- as.data.frame(t(dfpost48oxypatch))
dfpost48oxyplacebo <- as.data.frame(t(dfpost48oxyplacebo))

patch <- rep("patch", time = 11)
placebo <- rep("placebo" , time = 11)

sujet <- c(1, 2, 3, 4, 5,6, 7 , 8, 9 , 10, 11)

instant <- rep("post48", time = 11)

dfpost48oxypatch <- cbind(dfpost48oxypatch, patch, sujet, instant)
colnames(dfpost48oxypatch) <- c("smO2live" , "condition", "sujet" , "timing")

dfpost48oxyplacebo <- cbind(dfpost48oxyplacebo, placebo, sujet, instant)
colnames(dfpost48oxyplacebo) <- c("smO2live" , "condition", "sujet" , "timing")

dfpost48final <- rbind(dfpost48oxypatch, dfpost48oxyplacebo)

dfpost48final %>% ggplot(aes(x= condition , y = smO2live)) +
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


dfpost48oxyplaceboce <-
  as.data.frame(lapply(listdfpost48oxyplacebo , function(bob) {
    mean(bob[900:1020, ])
  }))

dfpost48oxypatchce <-
  as.data.frame(lapply(listdfpost48oxypatch , function(bob) {
    mean(bob[900:1020,])
  }))

dfpost48oxypatchce <- as.data.frame(t(dfpost48oxypatchce))
dfpost48oxyplaceboce <- as.data.frame(t(dfpost48oxyplaceboce))

dfpost48oxypatchce <- cbind(dfpost48oxypatchce, patch, sujet, instant)
colnames(dfpost48oxypatchce) <- c("smO2live" , "condition", "sujet" , "timing")

dfpost48oxyplaceboce <- cbind(dfpost48oxyplaceboce, placebo, sujet, instant)
colnames(dfpost48oxyplaceboce) <- c("smO2live" , "condition", "sujet" , "timing")

dfpost48finalce <- rbind(dfpost48oxypatchce, dfpost48oxyplaceboce)

dffinaloxy <- rbind(dfpost48final, dfpostfinal, dfprefinal)
dffinaloxyce <- rbind(dfpost48finalce, dfpostfinalce, dfprefinalce)

dffinaloxy$timing <- factor(dffinaloxy$timing, levels = c("PRE", "post", "post48"))
dffinaloxyce$timing <- factor(dffinaloxyce$timing, levels = c("PRE", "post", "post48"))

dfpost48final %>% group_by(condition) %>%  shapiro_test(smO2live)
dfpost48finalce %>% group_by(condition) %>% shapiro_test(smO2live)

dfpostfinal %>% group_by(condition) %>% shapiro_test(smO2live)
dfpostfinalce %>%  group_by(condition) %>% shapiro_test(smO2live)

dfprefinal %>% group_by(condition) %>%  shapiro_test(smO2live)
dfprefinalce %>% group_by(condition) %>% shapiro_test(smO2live)

res.aov1 <-
  dffinaloxyce %>% rstatix::anova_test(
    dv = smO2live,
    wid = sujet ,
    within = c(timing, condition) ,
    effect.size = "ges",
    detailed = TRUE,
  )

anova2 <- get_anova_table(res.aov1 , correction = "auto")

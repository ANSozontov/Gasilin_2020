# 0. License --------------------------------------------------------------
#
# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
#                          Artëm Sozontov ©
# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
# _________________________________________________________________________
# This code is stored on the GitHub repository 
# URL: https://github.com/ANSozontov/Gasilin_2020
# and has a CC BY SA 4.0 licence 
# URL: https://creativecommons.org/licenses/by-sa/4.0/ 
# 
#                         What does it mean? 
# 
# You are free to use, transform and distribute this code or its parts, 
# including commercial purposes. Just keep in mind only two restrictions: 
# `BY`: You are obligated to cite the article where authorship was claimed:
#     Gasilin V.V., Devjashin M.M., Plasteeva N.A., Sozontov A.N. 2021. 
#     Holocene variation in the species diversity and relative abundance 
#     of ungulates in the Eurasian forest steppe // Zoologichesky Zhurnal. 
#     Vol. 100. № 12. P. 1401–1421.
# `SA`: You are obligated to publish and distribute you derivate code 
#     under the license not stricter than the current one (CC-BY-SA). 
# _________________________________________________________________________

# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
#                          Артём Созонтов ©
# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
# _________________________________________________________________________
# Код размещен на репозитории GitHub 
# URL: https://github.com/ANSozontov/Gasilin_2020
# и распространяется под лицензией CC BY SA 4.0 
# URL: https://creativecommons.org/licenses/by-sa/4.0/ 
# 
#                         Что это означает? 
# 
# Вы можете свободно использовать, модифицировать и распространять этот код
# (включая коммерческое использование), учитывая лишь два ограничения:
# `BY`: Используя данный скрипт или его фрагменты вы обязаны процитировать 
#     статью, где было заявлено авторство: Гасилин В.В., Девяшин М.М., 
#     Пластеева Н.А., Созонтов А.Н. 2021. Изменения состава и относительного 
#     обилия копытных евразийской лесостепной зоны в голоцене // Зоологический 
#     журнал. Т. 100. № 12. С. 1401–1421.
# `SA`: Вы обязаны публиковать и распространять свой собственный код, 
#     производный от данного, под лицензией не строже чем текущая - СС-BY-SA
# _________________________________________________________________________



# LOAD --------------------------------------------------------------------
library(tidyverse)
library(vegan)
wide <- left_join(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data7.xlsx', sheet = "freq"), 
                  select(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data7.xlsx', sheet = "labs"), 
                         id, E, fs, foreststeppe, period2, n.species, n.bones), by = "id") %>% 
   as_tibble() %>% 
   filter(n.species > 1, n.bones > 4) %>% 
   select(-n.species, -n.bones)
paleo <- wide %>% 
   select(-"11.E_hydr", -foreststeppe) %>% 
   mutate(fs = case_when(fs == "10.Обская" ~ "9.Обская", fs != "10.Обская" ~ fs)) %>% 
   pivot_longer(names_to = "spec", values_to = "val", -c("id", "fs", "period2"))
recent <- rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data7.xlsx', sheet = "recent") %>% 
   select(-year, -region, -zone) %>% 
   pivot_longer(names_to = "spec", values_to = "val", -c("id", "fs", "period2"))

# DIST+ -------------------------------------------------------------------
dist <- vegan::vegdist(wide[,2:13], method="bray", binary=FALSE)
pcoa <- ape::pcoa(dist)
eig <- data.frame(eig = pcoa$values$Eigenvalues) %>% 
      filter(eig > 0) %>% 
      mutate(eig = round(eig/sum(eig)*100, 1)) %>% 
      pull(eig)
pcoa <- cbind(id = wide$id, pcoa$vectors[,1:2]) %>% 
      as_tibble() %>% 
      left_join(., select(wide, id, fs, foreststeppe, period2, E), by = "id")
plot2 <- gridExtra::grid.arrange(
   ggplot(filter(pcoa, fs != "islands"), 
          aes(x = Axis.1, y = Axis.2, shape = fs, color = fs)) + 
      geom_point(size = 2) + 
      stat_ellipse() +
      theme_bw() + 
      scale_shape_manual(values = c(1, 10, 0, 13, 2, 7, 6)) +
      theme(legend.position = "none") + 
      labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")),
   ggplot(filter(pcoa, period2 != "mix", fs != "islands"), 
          aes(x = Axis.1, y = Axis.2, shape = fs, color = fs)) + 
      geom_point() + 
      stat_ellipse(level = 0.8) +
      theme_bw() + 
      scale_shape_manual(values = c(1, 10, 0, 13, 2, 7, 6)) +
      theme(legend.position = "bottom") + 
      labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")) +
      facet_wrap(~period2), ncol = 1, heights = c(2, 1))
#ggsave("plot2.pdf", plot2, height = 8.5, width = 20/3, dpi = 1500)
plot2lab <- ggplot(filter(pcoa, fs != "islands"), 
      aes(x = Axis.1, y = Axis.2, shape = fs, color = fs)) + 
   geom_point() + 
   coord_fixed(ratio = 4) +
   stat_ellipse(level = 0.8) +
   theme_bw() + 
   #theme(legend.position = "bottom") +
   scale_shape_manual(values = c(1, 10, 0, 13, 2, 7, 6)) +
   labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)"))
#ggsave("plot2.legend.pdf", plot2lab, height = 10, width = 10, dpi = 3500)
plot3 <-  gridExtra::grid.arrange(
      ggplot(filter(pcoa, period2 != "mix"), aes(x = Axis.1, y = Axis.2, 
               linetype = period2, color = period2, shape = period2)) + 
            geom_point(size = 2) + 
            stat_ellipse(level = 0.8) +
            theme_bw() + 
            theme(legend.position = "none") + 
            #scale_color_manual(values = c("black", "grey40", "grey60"))+
            scale_shape_manual(values = c(1, 2, 0))+
            scale_linetype_manual(values = c("solid", "longdash", "dotdash"))+
            labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")),
      ggplot(filter(pcoa, period2 != "mix", foreststeppe != "11.Минусинская", 
                    foreststeppe != "12.Канская", foreststeppe != "6.Месягутовская"), 
             aes(x = Axis.1, y = Axis.2, linetype = period2, color = period2, shape = period2)) + 
         geom_point() + 
         stat_ellipse(level = 0.8) +
         theme_bw() + 
         theme(legend.position = "bottom") + 
         #scale_color_manual(values = c("black", "grey40", "grey60"))+
         scale_shape_manual(values = c(1, 2, 0))+
         scale_linetype_manual(values = c("solid", "longdash", "dotdash"))+
         labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")) + 
         facet_wrap(~foreststeppe), 
      ncol = 1, heights = c(1, 1.2))
#ggsave("plot3.pdf", plot3, height = 12.75*1.1, width = 10*1.1, dpi = 1500) 

# LM: linear & polynomial -------------------------------------------------
df <- pcoa %>% 
   rename(fs2= foreststeppe) %>% 
   mutate(E2 = E^2, 
          fs = case_when(fs2 == "1.Паннонская" ~ "01.Паннонская", 
                         fs2 == "2.Днепровская" ~ "02.Днепровская", 
                         fs2 == "3.Среднерусская" ~ "03.Среднерусская", 
                         fs2 == "5.Заволжская" ~ "05.Заволжская" , 
                         fs2 == "7.Тобольская" ~ "07.Тобольская", 
                         fs2 == "8.Ишимская" ~ "08.Ишимская", 
                         fs2 == "9.Барабинская" ~ "09.Барабинская", 
                         fs2 == "10.Обская" ~ "10.Обская",
                         fs2 == "13.Иркутская" ~ "13.Иркутская", 
                         TRUE ~ "islands"))
fit1 <- lm(Axis.1 ~ E, data = df)
fit2 <- lm(Axis.1 ~ E+E2, data = df)
summary(fit1)
AIC(fit1)
summary(fit2)
AIC(fit2)
dat <- data.frame(E = seq(17, 105, by = 1), 
                  pred = predict(fit2, mutate(data.frame(E = seq(17, 105, by = 1)), E2 = E^2)))
plot.6.add <- ggplot() + 
      geom_point(aes(x = E, y = Axis.1, color = fs, shape = fs), 
              data = filter(df, fs != "islands"))  + 
      geom_line(aes(x = E, y = pred), data = dat, 
             color = "green", size = 1) + 
      geom_line(aes(x = E, y = A1), data = data.frame(E = c(17, 105), 
            A1 = predict(fit1, data.frame(E = c(17, 105)))), color = "blue", size = 1) +
      theme_bw() + 
   theme(legend.position = "bottom")+
      scale_color_manual(values = c("#F8766D", "#00BA38", "#00C19F",
                                 "#00B9E3", "#DB72FB", "#FF61C3", 
                                 "#93AA00", "#D39200", "#619CFF")) +
      scale_shape_manual(values = c(1, 0, 13, 2, 7, 6, 9, 10, 14)) +
      coord_cartesian(xlim = c(17, 103), ylim = c(-0.6, 0.5))+
      labs(x = "Долгота", y = "Ось 1")
#ggsave("plot6.add.pdf", plot.6.add, height = 6, width = 10, dpi = 1500) 

# Correlation -------------------------------------------------------------
df2 <- right_join(wide, pcoa, by = "id")
res <- data.frame(spec = colnames(wide)[2:13], name = NA, RP = NA, PP = NA, RS = NA, PS = NA)
for(i in 1:nrow(res)) { 
   df3 <- select(df2, i+1, Axis.1)
   res$name[i] <- colnames(df3)[1]
   colnames(df3)[1] <- "spec"
   c <- cor.test(df3$spec, df3$Axis.1, method = "pearson")
   res$RP[i] <- c$estimate
   res$PP[i] <- c$p.value
   c <- cor.test(df3$spec, df3$Axis.1, method = "spearman")
   res$RS[i] <- c$estimate
   res$PS[i] <- c$p.value
   }
res <- res %>% 
   transmute(spec, rPearson = round(RP, 2), pvalPearson = round(PP, 4), levelPearson = case_when(
      pvalPearson < 0.001 ~"***",
      pvalPearson < 0.01 ~ "**",
      pvalPearson < 0.05 ~ "*", 
      pvalPearson < 0.1 ~ ".",
      TRUE ~ ""),
      rSpearman = round(RS, 2), pvalSpearman = round(PS, 4), levelSpearman = case_when(
      pvalSpearman < 0.001 ~"***",
      pvalSpearman < 0.01 ~ "**",
      pvalSpearman < 0.05 ~ "*", 
      pvalSpearman < 0.1 ~ ".",
      TRUE ~ "")
      ) %>% 
   mutate(pvalPearson = case_when(pvalPearson == 0 ~ "<0.0001", TRUE ~ as.character(pvalPearson)), 
          pvalSpearman = case_when(pvalSpearman == 0 ~ "<0.0001", TRUE ~ as.character(pvalSpearman)))
View(res)
#writexl::write_xlsx(res, "species.correlation.xlsx")

# Temporary graphs --------------------------------------------------------
# for(i in 1:11) { 
#    df3 <- select(df2, Axis.1, E.x, i+1)
#    j <- colnames(df3)[3]
#    colnames(df3)[2:3] <- c("Longitude" ,"spec")
#    p <- df3 %>% 
#       pivot_longer(names_to = "var", values_to = "val", -spec) %>% 
#       ggplot(aes(x = val, y = spec)) + 
#       geom_point(alpha = 0.7, shape = 21) + 
#       facet_wrap(~var, scales = "free") + 
#       theme_bw() + 
#       geom_smooth()+
#       labs(title = j, x = "", y = "Доля вида", subtitle = "Ось Х подписана в шапочке")
#    ggsave(paste0("tmp.", substr(j, 3, nchar(j)), ".png"), p, dpi = 1000) #@
# }

# boxplots ----------------------------------------------------------------
df <-  rbind(paleo, recent) %>% 
   mutate(fs = case_when(fs == "9.Обская" ~ "9.Бар.Обск", 
                         fs == "7.Barab.Ob" ~ "9.Бар.Обск", 
                         TRUE ~ fs)) %>% 
   filter(spec %in% c("1.Sus", "3.Cervus", "4.Capreol", "6.Alces"), period2 != "mix") %>% 
   group_by(id) %>% 
   mutate(val = val/sum(val)) %>% 
   ungroup() %>% 
   filter(val > 0, val != 1)
plot4 <- df %>% 
   ggplot(aes(x = period2, y = val)) + 
   geom_boxplot() + # width = -1) + 
   theme_bw() + 
   labs(x = "", y = "") + 
   facet_grid(cols = vars(spec), rows = vars(fs))
# ggsave("plot4.pdf", plot4, height = 11.5, width = 10, dpi = 1500) # top 4 species by foreststeppes

plot5 <- df %>% 
      ggplot(aes(x = period2, y = val)) + 
      geom_boxplot() + 
      theme_bw() + 
      labs(x = "", y = "") + 
      facet_wrap(~ spec)
# ggsave("plot5.pdf", plot5, height = 10, width = 10, dpi = 1500) # top 4 species in general



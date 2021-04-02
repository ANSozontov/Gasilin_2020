# Rem
# 1.Sus	1.Кабан
# 2.Mosch	2.Сибирская кабарга
# 3.Cervus	3.Благородный олень
# 4.Capreol	4.Косули
# 5.Megalo	5.Большерогий
# 6.Alces	6.Лось
# 7.Rangif	7.Северный олень
# 8.Bos_pr	8.Тур
# 9.Bison	9.Зубр
# 10.Saiga	10.Сайга
# 11.E_hydr	11.Европейский осел
# 12.E_hemi	12.Кулан

# LOAD --------------------------------------------------------------------
library(tidyverse)
library(vegan)
wide <- left_join(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data6.2.xlsx', sheet = "freq"), 
                  select(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data6.2.xlsx', sheet = "labs"), 
                         id, E, fs, foreststeppe, period2, n.species, n.bones), by = "id") %>% 
   as_tibble() %>% 
   filter(n.species > 1, n.bones > 4) %>% 
   select(-n.species, -n.bones)
paleo <- wide %>% 
   select(-"11.E_hydr", -foreststeppe) %>% 
   mutate(fs = case_when(fs == "10.Обская" ~ "9.Обская", fs != "10.Обская" ~ fs)) %>% 
   pivot_longer(names_to = "spec", values_to = "val", -c("id", "fs", "period2"))
recent <- rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data6.2.xlsx', sheet = "recent") %>% 
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

# LM (additional) ---------------------------------------------------------
# all fs 
fit1 <- pcoa %>% 
   lm(Axis.1 ~ E, data = .)
summary(fit1)
fit1 <- tibble(A1 = fit1$model$Axis.1, E = fit1$model$E,
               fitted1 = fit1$fitted.values, resid1 = fit1$residuals)
fit1 %>% ggplot(aes(x = E, y = resid1)) + geom_point()
fit1 %>% ggplot(aes(x = A1, y = resid1)) + geom_point()
# excluding Pannonskaya
fit2 <- pcoa %>% 
   mutate(fs = substr(fs, 1, 2)) %>% 
   filter(fs != "1.") %>% 
   lm(Axis.1 ~ E, data = .) 
summary(fit2)

fit <- pcoa %>% 
   mutate(E2 = E^2) %>% 
   lm(Axis.1 ~ E+E2, data = .)
dat <- data.frame(E = seq(17, 105, by = 1), 
   pred = predict(fit, mutate(data.frame(E = seq(17, 105, by = 1)), E2 = E^2)))
ggplot() + 
   geom_point(aes(x = E, y = Axis.1, shape = fs), data = pcoa) + 
   #geom_smooth(method = "lm", formula = "y ~ x") + 
   geom_line(aes(x = E, y = pred), data = dat, 
             color = "green", size = 1.5) + 
   theme_bw() + 
   theme(legend.position = "bottom")
   
tibble(E = pcoa$E, res = residuals(fit)) %>% 
   ggplot(aes(x = E, y = res)) + 
   geom_point()

# not linear? -------------------------------------------------------------
df <- pcoa %>% 
   # filter(foreststeppe != "6.Месягутовская", 
   #        foreststeppe != "11.Минусинская", 
   #        foreststeppe != "12.Канская") %>% 
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
dat <- data.frame(E = seq(17, 105, by = 1), 
                  pred = predict(fit2, mutate(data.frame(E = seq(17, 105, by = 1)), E2 = E^2)))
p <- ggplot() + 
   geom_point(aes(x = E, y = Axis.1, color = foreststeppe), data = df) + 
   geom_line(aes(x = E, y = pred), data = dat, 
             color = "green", size = 1.5) + 
   geom_line(aes(x = E, y = a), color = "blue", size = 1.5, data = 
                data.frame(E = c(20, 105)) %>% mutate(a = predict(fit1, .)))+
   theme_bw() + 
   labs(x = "Долгота", y = "Ось 1")
plotly::ggplotly(p)

plot.6.add <- ggplot() + 
   geom_point(aes(x = E, y = Axis.1, color = fs, shape = fs), 
              data = filter(df, fs != "islands"))  + 
   geom_line(aes(x = E, y = pred), data = dat, 
             color = "green", size = 1) + 
   theme_bw() + 
   theme(legend.position = "bottom")+
   scale_color_manual(values = c("#F8766D", "#00BA38", "#00C19F",
                                 "#00B9E3", "#DB72FB", "#FF61C3", 
                                 "#93AA00", "#D39200", "#619CFF"
   )) +
   scale_shape_manual(values = c(1, 0, 13, 2, 7, 6, 9, 10, 14)) +
   labs(x = "Долгота", y = "Ось 1") 
#ggsave("plot6.add.png", plot.6.add, height = 6, width = 10, dpi = 1500) 

# Correlation -------------------------------------------------------------
df <- full_join(wide, pcoa, by = "id")
res <- data.frame(spec = rep(NA,13), s.all = NA, p.all = NA, s.1.at = NA, p.1.at = NA, 
                      s.2.sb = NA, p.2.sb = NA, s.3.sa = NA, p.3.sa = NA)
for(i in 2:13) { 
   res$spec[i] <- colnames(df)[i]
   res$s.all[i] <- df %>% select(i, `Axis.1`) %>% cor(., method = "spearman") %>% .[2]
   res$p.all[i] <- df %>% select(i, `Axis.1`) %>% cor(., method = "pearson") %>% .[2]
   res$s.1.at[i] <- df %>% filter(period2.x == "1.AT") %>% select(i, `Axis.1`) %>% cor(., method = "spearman") %>% .[2]
   res$p.1.at[i] <- df %>% filter(period2.x == "1.AT") %>% select(i, `Axis.1`) %>% cor(., method = "pearson") %>% .[2]
   res$s.2.sb[i] <- df %>% filter(period2.x == "2.SB") %>% select(i, `Axis.1`) %>% cor(., method = "spearman") %>% .[2]
   res$p.2.sb[i] <- df %>% filter(period2.x == "2.SB") %>% select(i, `Axis.1`) %>% cor(., method = "pearson") %>% .[2]
   res$s.3.sa[i] <- df %>% filter(period2.x == "3.SA") %>% select(i, `Axis.1`) %>% cor(., method = "spearman") %>% .[2]
   res$p.3.sa[i] <- df %>% filter(period2.x == "3.SA") %>% select(i, `Axis.1`) %>% cor(., method = "pearson") %>% .[2]
   }
res <- res[-1,]
res[,-1] <- round(res[,-1], 3)
writexl::write_xlsx(res, "cor.spec_with_axis1.xlsx")

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



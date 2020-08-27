library(tidyverse)
library(vegan)

# dist+ -------------------------------------------------------------------
wide <- left_join(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5.xlsx', sheet = "freqlog"), 
          select(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5.xlsx', sheet = "labs"), 
                 id, fs, foreststeppe, period2, n.species, n.bones), by = "id") %>% 
      filter(n.species > 1, n.bones > 4) %>% 
      select(-n.species, -n.bones)
dist <- vegan::vegdist(wide[,2:13], method="bray", binary=FALSE)

pcoa <- ape::pcoa(dist)
eig <- data.frame(eig = pcoa$values$Eigenvalues) %>% 
      filter(eig > 0) %>% 
      mutate(eig = round(eig/sum(eig)*100, 1)) %>% 
      pull(eig)
pcoa <- cbind(id = wide$id, pcoa$vectors[,1:2]) %>% 
      as.data.frame %>% 
      left_join(., select(wide, id, fs, foreststeppe, period2), by = "id")

plot1 <-  gridExtra::grid.arrange(
      ggplot(filter(pcoa, period2 != "mix"), aes(x = Axis.1, y = Axis.2, color = period2, shape = period2)) + 
            geom_point() + 
            stat_ellipse(level = 0.8) +
            theme_bw() + 
            theme(legend.position = "none") + 
            labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")),
      ggplot(filter(pcoa, period2 != "mix", foreststeppe != "11.Минусинская", 
                    foreststeppe != "12.Канская", foreststeppe != "6.Месягутовская"), 
             aes(x = Axis.1, y = Axis.2, color = period2, shape = period2)) + 
         geom_point() + 
         stat_ellipse(level = 0.8) +
         theme_bw() + 
         theme(legend.position = "bottom") + 
         labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")) + 
         facet_wrap(~foreststeppe), 
      ncol = 1, heights = c(1, 1.2))
# ggsave("plot.1.png", plot1, height = 8.5, width = 20/3, dpi = 1500) 
# ggsave("plot.1.eps", plot1, height = 8.5, width = 20/3) 
# ggsave("plot.1.svg", plot1, height = 8.5, width = 20/3)

plot2 <- gridExtra::grid.arrange(
      ggplot(filter(pcoa, fs != "islands"), aes(x = Axis.1, y = Axis.2, color = fs, shape = fs)) + 
            geom_point() + 
            stat_ellipse(level = 0.8) +
            theme_bw() + 
            scale_shape_manual(values = c(1, 10, 0, 13, 2, 7, 6)) +
            theme(legend.position = "none") + 
            labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")),
      ggplot(filter(pcoa, period2 != "mix", fs != "islands"), aes(x = Axis.1, y = Axis.2, color = fs, shape = fs)) + 
            geom_point() + 
            stat_ellipse(level = 0.8) +
            theme_bw() + 
            scale_shape_manual(values = c(1, 10, 0, 13, 2, 7, 6)) +
            theme(legend.position = "bottom") + 
            labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)")) +
            facet_wrap(~period2),
      ncol = 1, heights = c(2, 1))
# ggsave("plot.2.png", plot2, height = 8.5, width = 20/3, dpi = 1500) # SVG, eps
# ggsave("plot.2.eps", plot2, height = 8.5, width = 20/3) # SVG, eps
# ggsave("plot.2.svg", plot2, height = 8.5, width = 20/3)

legend <- ggplot(filter(pcoa, fs != "islands"), aes(x = Axis.1, y = Axis.2, color = fs, shape = fs)) + 
   geom_point() + 
   stat_ellipse(level = 0.8) +
   theme_bw() + 
   scale_shape_manual(values = c(1, 10, 0, 13, 2, 7, 6)) +
   labs(x = paste0("Axis 1 (", eig[1], "%)"), y = paste0("Axis 2 (", eig[2], "%)"))
# ggsave("plot.2.legend.png", legend, height = 8.5, width = 20/3, dpi = 1500) # SVG, eps
# ggsave("plot.2.legend.eps", legend, height = 8.5, width = 20/3) # SVG, eps
# ggsave("plot.2.legend.svg", legend, height = 8.5, width = 20/3)


# boxplots ----------------------------------------------------------------
paleo <- left_join(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5.xlsx', sheet = "freqlog"), 
                  select(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5.xlsx', sheet = "labs"), id, fs, period2, n.species, n.bones), 
                  by = "id") %>% 
      filter(n.species > 1, n.bones > 4, fs != "islands" ) %>% 
      select(-n.species, -n.bones, -c("11.Европейский осел")) %>% 
      mutate(fs = case_when(fs == "10.Обская" ~ "9.Обская", fs != "10.Обская" ~ fs)) %>% 
      pivot_longer(names_to = "spec", values_to = "val", -c("id", "fs", "period2"))
recent <- select(readxl::read_xlsx("data5.xlsx", sheet = "recent.log"), -year, -region, -zone) %>% 
      pivot_longer(names_to = "spec", values_to = "val", -c("id", "fs", "period2"))
df <- rbind(paleo, recent) %>% 
      filter(spec %in% unique(recent$spec)) %>% 
      filter(period2 != "mix") %>% 
      group_by(id) %>% 
      mutate(val = val/sum(val)) %>% 
      ungroup() %>% 
      filter(val > 0, val != 1)
plot3 <- df %>% 
      ggplot(aes(x = period2, y = val)) + 
      geom_boxplot() + 
      theme_bw() + 
      labs(x = "", y = "") + 
      facet_wrap(~ spec)
# ggsave("plot.3.png", plot3, height = 10, width = 10, dpi = 1500)
# ggsave("plot.3.eps", plot3, height = 10, width = 10)
# ggsave("plot.3.svg", plot3, height = 10, width = 10)
plot4 <- df %>% 
      ggplot(aes(x = period2, y = val)) + 
      geom_boxplot() + # width = -1) + 
      theme_bw() + 
      labs(x = "", y = "") + 
      facet_grid(cols = vars(spec), rows = vars(fs))
# ggsave("plot.4.png", plot4, height = 11.5, width = 10, dpi = 1500) 
# ggsave("plot.4.eps", plot4, height = 11.5, width = 10)
# ggsave("plot.4.svg", plot4, height = 11.5, width = 10)

# Volynskaya --------------------------------------------------------------
vol <- rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/recent.xlsx', sheet = "recent") %>% 
   filter(zone == "лес Украины") %>% 
   select(-fs, -period2, -year, -region, -zone) %>% 
   pivot_longer(names_to = "spec", values_to = "val", -id) %>% 
   group_by(id) %>% 
   mutate(val = val/sum(val)) %>% 
   ungroup()

as.data.frame(rbind(summary(vol[vol$spec == unique(vol$spec)[1],]$val), 
                    summary(vol[vol$spec == unique(vol$spec)[2],]$val), 
                    summary(vol[vol$spec == unique(vol$spec)[3],]$val), 
                    summary(vol[vol$spec == unique(vol$spec)[4],]$val))) %>% 
   cbind(sp = unique(vol$spec), .) %>% 
   writexl::write_xlsx(., "volyn.data.boxplots.xlsx")

plot5 <- ggplot(vol, aes(x = spec, y = val)) + 
   geom_hline(yintercept = seq(0, 0.6, by = 0.1))+ 
   geom_boxplot() + 
   theme_bw() + 
   labs(x = "", y = "") +
   scale_y_continuous(minor_breaks = seq(0, 0.6, 0.01), breaks = seq(0, 0.6, by = 0.1))
# ggsave("plot.5.png", plot5, height = 11.5, width = 10, dpi = 2000)
# ggsave("plot.5.eps", plot5, height = 11.5, width = 10)

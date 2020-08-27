library(tidyverse) 

# obtain data5 ------------------------------------------------------------
allongdata <- rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5draft.xlsx', sheet = 1) %>% 
      group_by(id) %>% 
      mutate(freq = val/sum(val), freqlog = log1/sum(log1)) %>% 
      ungroup 
bin <- allongdata %>% 
      select(id, spec, bin) %>% 
      pivot_wider(names_from = spec, values_from = bin) %>% 
      mutate(`11.Европейский осел`  = case_when(is.na(`11.Европейский осел`) ~ 0,  
                                                !is.na(`11.Европейский осел`) ~ `11.Европейский осел`))
num <- left_join(allongdata, select(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5draft.xlsx', 
                                                sheet = "labs"), id, variable), by = "id") %>% 
      filter(variable == "numeric") %>% 
      select(id, spec, val) %>% 
      pivot_wider(names_from = spec, values_from = val) %>% 
      mutate(`11.Европейский осел`  = case_when(is.na(`11.Европейский осел`) ~ 0,  
                                                !is.na(`11.Европейский осел`) ~ `11.Европейский осел`))
freq <- left_join(allongdata, select(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5draft.xlsx',
                                                 sheet = "labs"), id, variable), by = "id") %>% 
      filter(variable == "numeric") %>% 
      select(id, spec, freq) %>% 
      pivot_wider(names_from = spec, values_from = freq) %>% 
      mutate(`11.Европейский осел`  = case_when(is.na(`11.Европейский осел`) ~ 0,  
                                                !is.na(`11.Европейский осел`) ~ `11.Европейский осел`))
freqlog <- left_join(allongdata, select(rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5draft.xlsx',
                                                    sheet = "labs"), id, variable), by = "id") %>% 
      filter(variable == "numeric") %>% 
      select(id, spec, freqlog) %>% 
      pivot_wider(names_from = spec, values_from = freqlog) %>% 
      mutate(`11.Европейский осел`  = case_when(is.na(`11.Европейский осел`) ~ 0,  
                                                !is.na(`11.Европейский осел`) ~ `11.Европейский осел`))
writexl::write_xlsx(list(allongdata = allongdata, 
                         bin = bin, 
                         num = num, 
                         freq = freq, 
                         freqlog = freqlog),
                    "data5.xlsx")
rm(list = ls())

# tables ------------------------------------------------------------------
t1 <- left_join(allongdata, rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5.xlsx', 
                            sheet = "labs"), by = "id")  %>% 
      select(spec, val, foreststeppe) %>% 
      group_by(spec, foreststeppe) %>% 
      summarise(nn = sum(val), .groups = 'drop') %>% 
      pivot_wider(names_from = foreststeppe, values_from = nn)

t2 <- left_join(allongdata, rio::import('https://github.com/ANSozontov/Gasilin_2020/raw/master/data5.xlsx',
                                        sheet = "labs"), by = "id")  %>% 
      select(spec, bin, foreststeppe) %>% 
      group_by(spec, foreststeppe) %>% 
      summarise(nbones = sum(bin), nn = length(bin), .groups = "drop") %>% 
      mutate(nnn = paste0(nbones, "/",nn), freq = round(nbones/nn*100))

writexl::write_xlsx(list(b.nones = t1,
                         points1 = pivot_wider(select(t2, spec, foreststeppe, nnn), 
                                               names_from = foreststeppe, values_from = nnn),
                         points2 = pivot_wider(select(t2, spec, foreststeppe, freq), 
                                               names_from = foreststeppe, values_from = freq)), path = "table.06.08.2020.xlsx")
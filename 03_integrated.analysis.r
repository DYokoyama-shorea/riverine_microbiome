rm(list =ls())

library(tidyverse)
library(lubridate)
library(vegan)
library(tidygraph)
library(ggraph)
library(corrr)
library(scales)
library(ggdendro)
library(patchwork)

CWD <- getwd()
SAVE_DIR <- str_c(CWD, "/save")
SAVE_DIR_FIG <- str_c(SAVE_DIR, "/fig")
if(file.exists(SAVE_DIR_FIG) == FALSE){
  dir.create(SAVE_DIR_FIG)
}

data_list <-
  read_rds(str_c(SAVE_DIR, "/data_list.rds", sep = ""))

list_map <- 
  read_rds(str_c(SAVE_DIR, "/list_map.rds", sep = ""))

DF_geo <- 
  read_rds(str_c(SAVE_DIR, "/DF_geo.rds", sep = "")) %>%
  dplyr::select(-c(latitude, longitude, C.link, D.link_Strahler)) %>%
  mutate(River = ifelse(River == "Pacific ocean", "Main stream", River)) %>%
  mutate(River = factor(River, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream", "Pacific ocean"))) %>%
  mutate(population_2020 = ifelse(is.na(population_2020), 0, population_2020))


Basic_info <-
  data_list$DF_info %>%
  left_join(., DF_geo, by = "ID") %>%
  rename(Soil_Wet.Andisols = "Soil_Wet_Andisols", 
         Soil_Brown.Forest.soils = "Soil_Brown_Forest_soils", 
         Soil_Brown.Lowland.soils = "Soil_Brown_Lowland_soils", 
         Soil_Grey.Lowland.soils = "Soil_Grey_Lowland_soils",
         Soil_Muck.soils = "Soil_Muck_soils", 
         Soil_Gley.soils = "Soil_Gley_soils", 
         Soil_Human.modified.soils = `Soil_Human-modified_soils`, 
         population.2020 = "population_2020") #%>%
#  mutate(River = ifelse(River == "Pacific ocean", "Main stream", River)) %>%
#  mutate(River = factor(River, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream")))

roiTable <- 
  data_list$roi.table %>%
  mutate(name = str_c("NMR_", Name, sep = "")) %>%
  mutate(label.mtb = ifelse(is.na(Annotation), Name, str_c(Name, Annotation, sep = ": ")))

### distance between two points
distance_each <-
  DF_geo %>% 
  dplyr::select(ID, distance, River) %>% 
  unnest() %>% 
  separate(to, into = c("delete", "to"), sep = "_") %>% 
  dplyr::select(-delete) %>%
  mutate(to = as.factor(as.double(to)))  %>%
  rename(from = "ID")


euclidean.distance_each <-
  DF_geo %>% 
  dplyr::select(ID, euclidean.distance) %>% 
  unnest() %>% 
  rename(from = "ID", euclidean.distance = "dist")


connectivity <-
  crossing(tibble(x = 33), 
           tibble(y = c(17, 18, 19, 20, 21, 22, 23))) %>%
  bind_rows(., crossing(tibble(x = c(17, 18, 19, 20, 21, 22, 23)),
                        tibble(y = 33))) %>%
  bind_rows(.,crossing(tibble(x = c(45, 46, 47)), 
                       tibble(y = c(42, 43, 44)))) %>%
  bind_rows(.,crossing(tibble(x = c(42, 43, 44)), 
                       tibble(y = c(45, 46, 47)))) %>%
  bind_rows(., crossing(tibble(x = DF_geo %>% filter(River == "Hayabuchi") %>% .$ID %>% as.double()), 
                        tibble(y = c(14, 15, 16, 3)))) %>%
  bind_rows(., crossing(tibble(y = DF_geo %>% filter(River == "Hayabuchi") %>% .$ID %>% as.double()), 
                        tibble(x = c(14, 15, 16, 3)))) %>%
  bind_rows(., crossing(tibble(x = DF_geo %>% filter(River == "Yagami") %>% .$ID %>% as.double()), 
                        tibble(y = c(13, 14, 15, 16, 3)))) %>%
  bind_rows(., crossing(tibble(y = DF_geo %>% filter(River == "Yagami") %>% .$ID %>% as.double()), 
                        tibble(x = c(13, 14, 15, 16, 3)))) %>%
  bind_rows(., crossing(tibble(x = DF_geo %>% filter(River == "Tsurumi") %>% .$ID %>% as.double()), 
                        tibble(y = DF_geo %>% filter(River == "Onda" | River == "Hayabuchi" | River == "Yagami") %>% .$ID %>% as.double()))) %>%
  bind_rows(., crossing(tibble(x = DF_geo %>% filter(River == "Onda") %>% .$ID %>% as.double()), 
                        tibble(y = DF_geo %>% filter(River == "Tsurumi" | River == "Hayabuchi" | River == "Yagami") %>% .$ID %>% as.double()))) %>%
  bind_rows(., crossing(tibble(x = DF_geo %>% filter(River == "Hayabuchi") %>% .$ID %>% as.double()), 
                        tibble(y = DF_geo %>% filter(River == "Onda" | River == "Tsurumi" | River == "Yagami") %>% .$ID %>% as.double()))) %>%
  bind_rows(., crossing(tibble(x = DF_geo %>% filter(River == "Yagami") %>% .$ID %>% as.double()), 
                        tibble(y = DF_geo %>% filter(River == "Onda" | River == "Tsurumi" | River == "Hayabuchi") %>% .$ID %>% as.double()))) %>%
  set_names(c("from", "to")) %>%
  mutate(from = as.factor(from),
         to = as.factor(to)) %>%
  mutate(connectivity = "disconnect")

distance_each_full <-
  left_join(distance_each, euclidean.distance_each, by = c("from", "to")) %>%
  left_join(., connectivity, by = c("from", "to")) %>%
  mutate(connectivity = ifelse(is.na(connectivity), "connect", connectivity)) %>%
  mutate(flow.distance = ifelse(connectivity == "connect", distance, NA))


x.axis.text.color <-
  Basic_info %>% 
  filter(filter == "1um") %>% 
  dplyr::select(ID_for_arrange, River) %>% 
  arrange(ID_for_arrange) %>%
  mutate(color = ifelse(River == "Tsurumi", hue_pal()(5)[1], 
                        ifelse(River == "Onda", hue_pal()(5)[2], 
                               ifelse(River == "Hayabuchi", hue_pal()(5)[3], 
                                      ifelse(River == "Yagami", hue_pal()(5)[4], 
                                             ifelse(River == "Main stream", hue_pal()(5)[5],
                                                    ifelse(River == "Pacific ocean", "grey40", NA_character_))))))) %>%
  .$color

plot_landuse <-
  Basic_info %>%
  filter(filter == "1um") %>%
  select_if(names(.) %in% c("ID", "ID_for_arrange", "River") | names(.) %>% str_sub(1,7) == "Landuse") %>%
  gather(-c(ID, ID_for_arrange, River), key = "Landuse", value = "prop") %>%
  ggplot(aes(x = ID_for_arrange, y= prop * 100, fill = Landuse)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(Landuse_city.cover = "grey80", Landuse_forest.cover = "#006f3c", Landuse_field.cover = "#f9a73e")) +
  theme_bw() +
  labs(x = "Site", y = "Proportion (%)") +
  theme(axis.text.x = element_text(angle = 45, color = x.axis.text.color)) 

plot_soil <-
  Basic_info %>%
  filter(filter == "1um") %>%
  select_if(names(.) %in% c("ID", "ID_for_arrange", "River") | names(.) %>% str_sub(1,4) == "Soil") %>%
  gather(-c(ID, ID_for_arrange, River), key = "soil.type", value = "prop") %>%
  mutate(soil.type = str_sub(soil.type, 6, -1)) %>%
  mutate(soil.type = factor(soil.type, 
                            levels = c("Others", 
                                       "Andisols", "Wet.Andisols", 
                                       "Brown.Forest.soils", "Brown.Lowland.soils", "Grey.Lowland.soils",
                                       "Muck.soils", "Gley.soils", 
                                       "Human.modified.soils"))) %>%
  ggplot(aes(x = ID_for_arrange, y= prop * 100, fill = soil.type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = append(append("grey80", hue_pal()(7)), "grey60")) +
  theme_bw() +
  labs(x = "Site", y = "Proportion (%)") +
  theme(axis.text.x = element_text(angle = 45, color = x.axis.text.color)) 

plot_geo <-
  Basic_info %>%
  filter(filter == "1um") %>%
  select_if(names(.) %in% c("ID", "ID_for_arrange", "River") | names(.) %>% str_sub(1,7) == "Geology") %>%
  gather(-c(ID, ID_for_arrange, River), key = "rock", value = "prop") %>%
  mutate(rock = str_sub(rock, 9, -1)) %>%
  mutate(rock = factor(rock, levels = c("Others", "Volcanic", "Mudstone", "Gravel", "Sand", "Mud"))) %>%
  ggplot(aes(x = ID_for_arrange, y= prop * 100, fill = rock)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = append("grey80", hue_pal()(5))) +
  theme_bw() +
  labs(x = "Site", y = "Proportion (%)") +
  theme(axis.text.x = element_text(angle = 45, color = x.axis.text.color)) 

plot_population <-
  Basic_info %>%
  filter(filter == "1um") %>%
  select_if(names(.) %in% c("ID", "ID_for_arrange", "River", "population.2020")) %>%
  ggplot(aes(x = ID_for_arrange, y= population.2020)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Site", y = expression(paste("Population density (", km^{-2}, ")"))) +
  theme(axis.text.x = element_text(angle = 45, color = x.axis.text.color)) 


map_plot_landuse <- list_map[[3,2]][[1]] / plot_landuse + theme(legend.position = "none") + plot_layout(heights = c(1,1))
map_plot_geo <- list_map[[4,2]][[1]] / plot_geo + theme(legend.position = "none") + plot_layout(heights = c(1,1))
map_plot_soil <- list_map[[5,2]][[1]] / plot_soil + theme(legend.position = "none") + plot_layout(heights = c(1,1))
map_plot_population <- list_map[[6,2]][[1]] / plot_population + theme(legend.position = "none") + plot_layout(heights = c(1,1))
ggsave(plot = map_plot_landuse, 
       file = str_c(SAVE_DIR, "/Mapping/map_plot_landuse.png"),
       width = 9, height = 10)
ggsave(plot = map_plot_geo, 
       file = str_c(SAVE_DIR, "/Mapping/map_plot_geo.png"),
       width = 9, height = 10)
ggsave(plot = map_plot_soil, 
       file = str_c(SAVE_DIR, "/Mapping/map_plot_soil.png"),
       width = 9, height = 10)
ggsave(plot = map_plot_population, 
       file = str_c(SAVE_DIR, "/Mapping/map_plot_population.png"),
       width = 9, height = 10)



DF_microbe.2 <-
  data_list$DF_microbe %>%
  mutate(data3 = map(data3, 
                     function(x){
                       x %>%
                         arrange(ID, filter)
                     })) %>%
  mutate(vegdist = map(data3,
                       function(x){
                         x %>%
                           dplyr::select(-c(ID, filter)) %>%
                           vegdist(method = "bray") 
                       })) %>%
  mutate(df_vegdist = map2(data3, vegdist,
                           function(x, y){
                             name.tmp <- 
                               x %>%
                               dplyr::select(ID, filter) %>%
                               mutate(ID2 = str_c(ID, filter, sep = "_")) %>%
                               .$ID2
                               
                             y %>%
                               as.matrix() %>%
                               as_tibble() %>%
                               set_names(name.tmp) %>%
                               bind_cols(tibble(from = name.tmp),.) %>%
                               gather(-from, key = "to", value = "dissimilarity") %>%
                               separate(from, into = c("from", "from_filter"), sep = "_") %>%
                               separate(to, into = c("to", "to_filter"), sep = "_") %>%
                               mutate(from = as.factor(as.double(from)),
                                      to = as.factor(as.double(to))) %>%
                               left_join(., distance_each_full, by = c("from", "to")) %>%
                               left_join(., DF_geo %>% dplyr::select(ID, D.link_Shreve, River) %>% set_names(str_c("from", names(.),  sep= "_")) %>% rename(from = "from_ID"), by = "from") %>%
                               left_join(., DF_geo %>% dplyr::select(ID, D.link_Shreve, River) %>% set_names(str_c("to", names(.),  sep= "_")) %>% rename(to = "to_ID"), by = "to") %>%
                               mutate(filter.pair = map2_chr(from_filter, to_filter, function(x,y){append(x,y) %>% sort() %>% str_c(collapse = "_")}))  %>%
                               mutate(river.pair = map2_chr(from_River, to_River, function(x,y){append(x,y) %>% sort() %>% str_c(collapse = "_")})) %>%
                               mutate(river.pair.2 = ifelse(from_River == to_River, "Same", "Different")) %>%
                               mutate(river.pair.3 = ifelse(from_River == to_River, "Same", 
                                                            ifelse((from_River %in% c("Main stream", "Pacific ocean")|to_River %in% c("Main stream", "Pacific ocean")), "Upstream - Downstream", 
                                                                   "Upstream - Upstream"))) %>%
                               mutate(river.pair.4 = ifelse((from_River == "Main stream"|to_River == "Main stream"), "With Main stream", 
                                                                   "Without Main stream")) %>%
                               mutate(with.spring.water = map2_lgl(from, to, function(x, y){append(x, y) %in% c(21, 27) %>% any()})) 
                           })) %>%
  mutate(plot_dissimilarity = map(df_vegdist,
                                  function(x){
                                    x %>%
                                      filter(dissimilarity  != 0) %>%
                                      filter(filter.pair != "0.2um_1um") %>%
                                      mutate(filter.pair = ifelse(filter.pair == "0.2um_0.2um", "FL", "PA") %>% factor(., levels = c("PA", "FL"))) %>%
                                      mutate(River = factor(River, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream"))) %>%
                                      #ggplot(aes(x = log(distance), y = dissimilarity, group = from, color = connectivity)) +
                                      ggplot(aes(x = distance, y = 1 - dissimilarity,  color = river.pair.4)) +
                                      geom_point(alpha = 0.05) +
                                      facet_grid(filter.pair ~ .) +
                                      #facet_grid(filter.pair ~ river.pair.3) +
                                      geom_smooth(method = "lm", se = F) +
                                      #scale_color_manual(values = c("Tsurumi" = hue_pal()(5)[1], 
                                      #                              "Onda" = hue_pal()(5)[2], 
                                      #                              "Hayabuchi" = hue_pal()(5)[3], 
                                      #                              "Yagami" = hue_pal()(5)[4], 
                                      #                              "Main stream" = hue_pal()(5)[5])) +
                                      theme_bw() +
                                      labs(x = "Distance (km)", y = "Similarity (1 - Bray-Curtis Index)", color = "Tributary pair") +
                                      ylim(0, 1)
                                    
                                  })) %>%
  mutate(nmds = map(data3, 
                    function(x){
                      set.seed(1234)
                      x %>%
                        dplyr::select(-ID, -filter) %>%
                        metaMDS(k = 2, trymax = 20)
                    })) %>%
  mutate(df.nmds = map2(data3, nmds,
                          function(x, y){
                            x %>%
                              dplyr::select(ID, filter) %>%
                              bind_cols(., y$points %>% as_tibble()) %>%
                              left_join(., Basic_info, by = c("ID", "filter"))
                          })) %>%
  mutate(plot.mds = map(df.nmds,
                        function(x){
                          x %>%
                            mutate(filter = ifelse(filter == "0.2um", "FL", "PA") %>% factor(., levels = c("PA", "FL"))) %>%
                            ggplot(aes(x = MDS1, y = MDS2, color = River, shape = filter, size = D.link_Shreve)) +
                            geom_point(alpha = 0.6) +
                            scale_color_manual(values = c("Tsurumi" = hue_pal()(5)[1], 
                                                          "Onda" = hue_pal()(5)[2], 
                                                          "Hayabuchi" = hue_pal()(5)[3], 
                                                          "Yagami" = hue_pal()(5)[4], 
                                                          "Main stream" = hue_pal()(5)[5])) +
                            theme_bw() +
                            labs(x = "nMDS1", y = "nMDS2", shape = "Filter") +
                            facet_grid(filter ~ .)
                        })) %>%
  mutate(permanova = map(data3,
                         function(x){
                           data.tmp <- x %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
                           target.tmp <- data.tmp %>% dplyr::select(-c(ID, filter, River))
                           filter.tmp <- data.tmp %>% dplyr::select(filter) %>% .[[1]]
                           River.tmp <- data.tmp %>% dplyr::select(River) %>% .[[1]]
                           adonis(target.tmp ~ filter.tmp * River.tmp)
                         })) %>%
#  mutate(pairwise.permanova = map(data3,
#                                  function(x){
#                                    data.tmp <- x %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
#                                    TMP <- data.tmp  %>% select(-c(ID, filter, River)) %>% as.data.frame()
#                                    TMP2 <- data.tmp  %>% select(c(ID, filter, River)) %>% as.data.frame()
#                                    pairwise.adonis2(TMP ~ filter + River, data = TMP2)
#                                  })) %>%
  mutate(bar.plot = map(data3, 
                        function(x){
                          tmp.1 <-
                            x %>%
                            left_join(Basic_info %>% dplyr::select(ID, ID_for_arrange, filter, D.link_Shreve, distance.from.ocean, River), ., by = c("ID", "filter")) %>%
                            arrange(River, desc(distance.from.ocean)) %>%
                            #nest(-filter) %>%
                            #mutate(data = map(data, function(x){x %>% mutate(ID2 = row_number() %>% as.factor())})) %>%
                            #unnest() %>%
                            gather(-c(ID, ID_for_arrange, filter, D.link_Shreve, distance.from.ocean, River), key = "taxa", value = "prop") %>%
                            nest(-taxa) %>%
                            mutate(max.prop = map_dbl(data, ~max(.$prop, na.rm = T))) %>%
                            arrange(max.prop)
                          
                          tmp.1 <-
                            bind_rows(tmp.1 %>% filter(taxa == "Unassigned"), 
                                      tmp.1 %>% filter(taxa != "Unassigned"))  %>%
                            mutate(taxa = factor(taxa, levels = .$taxa)) %>%
                            unnest()
                          
                          tmp.2 <- tmp.1$taxa %>% unique() %>% length()
                          
                          palette <-
                            hue_pal(c = 50)(tmp.2) %>%
                            as_tibble() %>%
                            sample_n(tmp.2) %>%
                            .[[1]]
                          palette_top12 <-
                            hue_pal()(12)
                          
                          palette[1] <- "grey"
                          for(col in 1:length(palette_top12)){
                            palette[tmp.2 - col + 1] <- palette_top12[col] 
                          }
                          
                          x.axis.color <-
                            tmp.1 %>% nest(-ID_for_arrange, -River) %>% 
                            mutate(color = ifelse(River == "Tsurumi", hue_pal()(5)[1], 
                                                  ifelse(River == "Onda", hue_pal()(5)[2], 
                                                         ifelse(River == "Hayabuchi", hue_pal()(5)[3], 
                                                                ifelse(River == "Yagami", hue_pal()(5)[4], 
                                                                       ifelse(River == "Main stream", hue_pal()(5)[5],NA_character_)))))) %>%
                            .$color
                          
                          res <-
                            tmp.1 %>%
                            mutate(filter = ifelse(filter == "0.2um", "FL", "PA") %>% factor(., levels = c("PA", "FL"))) %>%
                            ggplot(aes(x = ID_for_arrange, y = prop * 100, fill = taxa)) +
                            geom_bar(stat = "identity") +
                            facet_grid(filter ~.) +
                            scale_fill_manual(values = palette) +
                            theme_bw() +
                            theme(axis.text.x = element_text(angle = 45, color = x.axis.color),
                                  legend.position = "none") +
                            labs(x = "Site", y = "Proportion (%)")
                          
                          return(res)
                        })) %>%
  mutate(bar.plot.label = map2(bar.plot, num,
                               function(x, y){
                                 if (y %in% c(1,2,3,4,5,6)){
                                   tmp <-
                                     x$data %>%
                                     nest(-taxa) %>% 
                                     arrange(desc(taxa)) %>%
                                     .[(1:12),1] %>%
                                     mutate(taxa.split = str_split(taxa, pattern = "; ")) %>%
                                     mutate(taxa2 = map_chr(taxa.split, ~.[y])) 
                                   
                                   tmp2 <-
                                     tmp %>%
                                     mutate(taxa2 = factor(taxa2, levels = tmp$taxa2)) %>%
                                     ggplot(aes(x = taxa, y = taxa2, fill = taxa2)) +
                                     geom_tile() +
                                     labs(fill = "Top 12 taxa")
                                   
                                   res <-
                                     tmp2 %>%
                                     ggpubr::get_legend() %>% 
                                     ggpubr::as_ggplot()
                                   
                                 } else
                                 {res <- NA}
                                 return(res)
                               }))

#  mutate(pairwise.permanova = map(data3,
#                                  function(x){
#                                    data.tmp <- x %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
#                                    TMP <- data.tmp  %>% select(-c(ID, filter, River)) %>% as.data.frame()
#                                    TMP2 <- data.tmp  %>% select(c(ID, filter, River)) %>% as.data.frame()
#                                    pairwise.adonis2(TMP ~ filter + River, data = TMP2)
#                                  }))

library(pairwiseAdonis)
permanova.res.tmp <- list()
for(i in 1:nrow(DF_microbe.2)){
  data.tmp <- DF_microbe.2[i,] %>% dplyr::select(data3) %>% unnest() %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
  TMP <- data.tmp  %>% dplyr::select(-c(ID, filter, River)) %>% as.data.frame()
  TMP2 <- data.tmp  %>% dplyr::select(c(ID, filter, River)) %>% as.data.frame()
  res <- pairwise.adonis2(TMP ~  River * filter, data = TMP2)
  permanova.res.tmp <- append(permanova.res.tmp, list(res))
}
DF_microbe.3 <-
  DF_microbe.2 %>%
  bind_cols(., tibble(pairwise.permanova = permanova.res.tmp)) %>%
  mutate(pairwise.permanova = map(pairwise.permanova,
                                  function(x){
                                    tibble(pair = names(x),
                                           result = x) %>%
                                      filter(pair != "parent_call") %>%
                                      mutate(result = map(result,
                                                          function(x){
                                                            as_tibble(x, rownames = "factor")
                                                          }))
                                  })) 



asv.permanova <-
  DF_microbe.2[8,] %>%
  dplyr::select(data3, permanova) %>%
  mutate(permanova.2 = map(data3,
                           function(x){
                             data.tmp <- x %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
                             res <-
                               left_join(data_list$DF_ICP, data_list$DF_water, by = "ID") %>%
                               gather(-ID, key = "param", value = "val") %>%
                               nest(-param) %>%
                               mutate(permanova = map(data,
                                                      function(xx){
                                                        data.tmp.2 <-
                                                          data.tmp %>%
                                                          left_join(., xx, by = "ID")
                                                        target.tmp <- data.tmp.2 %>% dplyr::select(-c(ID, filter, River, val))
                                                        filter.tmp <- data.tmp.2 %>% dplyr::select(filter) %>% .[[1]]
                                                        River.tmp <- data.tmp.2 %>% dplyr::select(River) %>% .[[1]]
                                                        val.tmp <- data.tmp.2 %>% dplyr::select(val) %>% .[[1]]
                                                        adonis(target.tmp ~ filter.tmp * River.tmp * val.tmp)
                                                        
                                                      }))
                             return(res)
                           })) %>%
  mutate(permanova.3 = map(data3,
                           function(x){
                             data.tmp <- x %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
                             pair.tmp <-
                               left_join(data_list$DF_ICP, data_list$DF_water, by = "ID") %>%
                               gather(-ID, key = "param", value = "val") %>%
                               mutate(param = str_replace(param, "Cd", "EC")) %>%
                               nest(-param) 
                             
                             pair.tmp.2 <-
                               crossing(pair.tmp$param,pair.tmp$param,) %>%
                               set_names(c("param1", "param2")) %>%
                               filter(param1 != param2) %>%
                               left_join(., pair.tmp %>% set_names(c("param1", "data1")), by = "param1") %>%
                               left_join(., pair.tmp %>% set_names(c("param2", "data2")), by = "param2") %>%
                               mutate(data3 = map2(data1, data2, function(x,y){left_join(x, y, by = "ID") %>% set_names("ID", "val1", "val2")})) %>%
                               dplyr::select(-data1, -data2)
                             
                             res <-
                               pair.tmp.2 %>%
                               mutate(num = row_number()) %>%
                               mutate(permanova = map2(data3, num,
                                                       function(xx, yy){
                                                         data.tmp.2 <-
                                                           data.tmp %>%
                                                           left_join(., xx, by = "ID")
                                                         target.tmp <- data.tmp.2 %>% dplyr::select(-c(ID, filter, River, val1, val2))
                                                         filter.tmp <- data.tmp.2 %>% dplyr::select(filter) %>% .[[1]]
                                                         River.tmp <- data.tmp.2 %>% dplyr::select(River) %>% .[[1]]
                                                         val1.tmp <- data.tmp.2 %>% dplyr::select(val1) %>% .[[1]]
                                                         val2.tmp <- data.tmp.2 %>% dplyr::select(val2) %>% .[[1]]
                                                         res.adonis <- 
                                                           adonis(target.tmp ~ filter.tmp * River.tmp * val1.tmp * val2.tmp) %>% .[[1]] %>%
                                                           as_tibble(rownames = "factor")
                                                           
                                                        
                                                         print(str_c(yy, " / ", nrow(pair.tmp.2)))
                                                         return(res.adonis)
                                                       }))
                             return(res)
                           }))

DF_nmr.2 <-
  data_list$DF_nmr[2,] %>% 
  mutate(vegdist = map(data,
                       function(x){
                         x %>%
                           dplyr::select(-c(ID, filter)) %>%
                           vegdist(method = "bray") 
                       })) %>%
  mutate(df_vegdist = map2(data, vegdist,
                           function(x, y){
                             name.tmp <- 
                               x %>%
                               dplyr::select(ID, filter) %>%
                               mutate(ID2 = str_c(ID, filter, sep = "_")) %>%
                               .$ID2
                             
                             y %>%
                               as.matrix() %>%
                               as_tibble() %>%
                               set_names(name.tmp) %>%
                               bind_cols(tibble(from = name.tmp),.) %>%
                               gather(-from, key = "to", value = "dissimilarity") %>%
                               separate(from, into = c("from", "from_filter"), sep = "_") %>%
                               separate(to, into = c("to", "to_filter"), sep = "_") %>%
                               mutate(from = as.factor(as.double(from)),
                                      to = as.factor(as.double(to))) %>%
                               left_join(., distance_each, by = c("from", "to")) %>%
                               left_join(., DF_geo %>% dplyr::select(ID, D.link_Shreve, River) %>% set_names(str_c("from", names(.),  sep= "_")) %>% rename(from = "from_ID"), by = "from") %>%
                               left_join(., DF_geo %>% dplyr::select(ID, D.link_Shreve, River) %>% set_names(str_c("to", names(.),  sep= "_")) %>% rename(to = "to_ID"), by = "to") %>%
                               mutate(filter.pair = map2_chr(from_filter, to_filter, function(x,y){append(x,y) %>% sort() %>% str_c(collapse = "_")}))  %>%
                               mutate(river.pair = map2_chr(from_River, to_River, function(x,y){append(x,y) %>% sort() %>% str_c(collapse = "_")})) %>%
                               mutate(river.pair.2 = ifelse(from_River == to_River, "Same", "Different")) %>%
                               mutate(river.pair.3 = ifelse(from_River == to_River, "Same", 
                                                            ifelse((from_River %in% c("Main stream", "Pacific ocean")|to_River %in% c("Main stream", "Pacific ocean")), "Upstream - Downstream", 
                                                                   "Upstream - Upstream"))) %>%
                               mutate(river.pair.4 = ifelse((from_River == "Main stream"|to_River == "Main stream"), "With Main stream", 
                                                            "Without Main stream")) %>%
                               mutate(with.spring.water = map2_lgl(from, to, function(x, y){append(x, y) %in% c(21, 27) %>% any()}))
                           })) %>%
  mutate(plot_dissimilarity = map(df_vegdist,
                                  function(x){
                                    x %>%
                                      filter(dissimilarity  != 0) %>%
                                      filter(filter.pair != "0.2um_1um") %>%
                                      mutate(filter.pair = ifelse(filter.pair == "0.2um_0.2um", "FOM", "POM") %>% factor(., levels = c("POM", "FOM"))) %>%
                                      mutate(River = factor(River, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream"))) %>%
                                      #ggplot(aes(x = log(distance), y = dissimilarity, group = from, color = connectivity)) +
                                      ggplot(aes(x = distance, y = 1 - dissimilarity,  color = river.pair.4)) +
                                      geom_point(alpha = 0.05) +
                                      facet_grid(filter.pair ~ river.pair.4) +
                                      #facet_grid(filter.pair ~ river.pair.3) +
                                      geom_smooth(method = "lm", se = F) +
                                      #scale_color_manual(values = c("Tsurumi" = hue_pal()(5)[1], 
                                      #                              "Onda" = hue_pal()(5)[2], 
                                      #                              "Hayabuchi" = hue_pal()(5)[3], 
                                      #                              "Yagami" = hue_pal()(5)[4], 
                                      #                              "Main stream" = hue_pal()(5)[5])) +
                                      theme_bw() +
                                      labs(x = "Distance (km)", y = "Similarity (1 - Bray-Curtis Index)", color = "Tributary pair") +
                                      ylim(0, 1)
                                    
                                    
                                  })) %>%
  mutate(nmds = map(data, 
                    function(x){
                      set.seed(1234)
                      x %>%
                        dplyr::select(-ID, -filter) %>%
                        metaMDS(k = 2, trymax = 20)
                    })) %>%
  mutate(df.nmds = map2(data, nmds,
                        function(x, y){
                          x %>%
                            dplyr::select(ID, filter) %>%
                            bind_cols(., y$points %>% as_tibble()) %>%
                            left_join(., Basic_info, by = c("ID", "filter"))
                        })) %>%
  mutate(plot.mds = map(df.nmds,
                        function(x){
                          x %>%
                            mutate(filter = ifelse(filter == "0.2um", "FOM", "POM") %>% factor(., levels = c("POM", "FOM"))) %>%
                            ggplot(aes(x = MDS1, y = MDS2, color = River, shape = filter, size = D.link_Shreve)) +
                            geom_point(alpha = 0.7) +
                            scale_color_manual(values = c("Tsurumi" = hue_pal()(5)[1], 
                                                          "Onda" = hue_pal()(5)[2], 
                                                          "Hayabuchi" = hue_pal()(5)[3], 
                                                          "Yagami" = hue_pal()(5)[4], 
                                                          "Main stream" = hue_pal()(5)[5])) +
                            theme_bw() +
                            labs(x = "nMDS1", y = "nMDS2", shape = "Filter") +
                            facet_grid(filter ~ .)
                        })) %>%
  mutate(bar.plot = map(data, 
                        function(x){
                          tmp.1 <-
                            x %>%
                            left_join(Basic_info %>% dplyr::select(ID, ID_for_arrange, filter, D.link_Shreve, distance.from.ocean, River), ., by = c("ID", "filter")) %>%
                            arrange(River, desc(distance.from.ocean)) %>%
                            gather(-c(ID, ID_for_arrange, filter, D.link_Shreve, distance.from.ocean, River), key = "ROI", value = "prop") %>%
                            nest(-ROI) %>%
                            mutate(max.prop = map_dbl(data, ~max(.$prop, na.rm = T))) %>%
                            arrange(max.prop) %>% 
                            mutate(ROI = factor(ROI, levels = .$ROI)) %>%
                            unnest()
                          
                          tmp.2 <- tmp.1$ROI %>% unique() %>% length()

                          palette <-
                            hue_pal(c = 50)(tmp.2) %>%
                            as_tibble() %>%
                            sample_n(tmp.2) %>%
                            .[[1]]
                          palette_top12 <-
                            hue_pal()(12)
                          
                          for(col in 1:length(palette_top12)){
                            palette[tmp.2 - col + 1] <- palette_top12[col] 
                          }
                          
                          x.axis.color <-
                            tmp.1 %>% nest(-ID_for_arrange, -River) %>% 
                            mutate(color = ifelse(River == "Tsurumi", hue_pal()(5)[1], 
                                                  ifelse(River == "Onda", hue_pal()(5)[2], 
                                                         ifelse(River == "Hayabuchi", hue_pal()(5)[3], 
                                                                ifelse(River == "Yagami", hue_pal()(5)[4], 
                                                                       ifelse(River == "Main stream", hue_pal()(5)[5],
                                                                              ifelse(River == "Pacific ocean", "grey40", NA_character_))))))) %>%
                            .$color
                          
                          res <-
                            tmp.1 %>%
                            mutate(filter = ifelse(filter == "0.2um", "FOM", "POM") %>% factor(., levels = c("POM", "FOM"))) %>%
                            ggplot(aes(x = ID_for_arrange, y = prop * 100, fill = ROI)) +
                            geom_bar(stat = "identity") +
                            facet_grid(filter ~.) +
                            scale_fill_manual(values = palette) +
                            theme_bw() +
                            theme(axis.text.x = element_text(angle = 45, color = x.axis.color),
                                  legend.position = "none") +
                            labs(x = "Site", y = "Proportion (%)")
                          
                          return(res)
                        })) %>%
  mutate(bar.plot.label = map(bar.plot,
                              function(x){
                                tmp <-
                                  x$data %>%
                                  nest(-ROI) %>% 
                                  arrange(desc(ROI)) %>%
                                  .[(1:12),1] %>%
                                  left_join(. ,roiTable %>% rename(ROI = "Name"), by = "ROI") %>%
                                  mutate(label.mtb.2 = ifelse(!is.na(Annotation), str_c(cs, " ppm; ", Annotation), str_c(cs, "ppm")))
                                
                                tmp2 <-
                                  tmp %>%
                                  mutate(label.mtb.2 = factor(label.mtb.2, levels = tmp$label.mtb.2)) %>%
                                  ggplot(aes(x = ROI, y = ROI, fill = label.mtb.2)) +
                                  geom_tile() +
                                  labs(fill = "Top 12 ROI")
                                
                                res <-
                                  tmp2 %>%
                                  ggpubr::get_legend() %>% 
                                  ggpubr::as_ggplot()
                                
                                
                                return(res)
                              })) %>%
  mutate(permanova = map(data,
                         function(x){
                           data.tmp <- x %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
                           target.tmp <- data.tmp %>% dplyr::select(-c(ID, filter, River))
                           filter.tmp <- data.tmp %>% dplyr::select(filter) %>% .[[1]]
                           River.tmp <- data.tmp %>% dplyr::select(River) %>% .[[1]]
                           adonis(target.tmp ~ filter.tmp * River.tmp)
                         })) %>%
  mutate(cluster = map2(vegdist, data,
                       function(x, y){
                         h <- hclust(x, method = "ward.D2")
                         d <- as.dendrogram(h)
                         dd <- dendro_data(d, type = "rectangle")
                         
                         label <- 
                           y %>% dplyr::select(ID, filter) %>% mutate(label = as.character(row_number())) %>%
                           left_join(., Basic_info %>% dplyr::select(ID, filter, River, D.link_Shreve, distance.from.ocean), by = c("ID", "filter"))
                         dd$labels <-
                           dd$labels %>%
                           left_join(., label, by = "label")
                      
                         return(dd)
                       })) %>%
  mutate(plot.cluster = map(cluster,
                            function(x){
                              ggplot(segment(x)) +
                                geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
                                geom_point(data = x$labels, aes(x, y, label = label, color = River, shape = filter),size = 3, hjust = 1, angle = 90) +
                                theme_bw()
                            })) %>%
  mutate(cluster.roi = map(data,
                           function(x){
                             x %>%
                               nest(-filter) %>%
                               mutate(data = map(data, function(xx){xx %>% gather(-ID, key = "roi", value = "prop") %>%
                                   group_by(roi) %>%
                                   mutate(max = max(prop), min = min(prop)) %>%
                                   mutate(val = (prop - min)/(max - min)) %>%
                                   dplyr::select(-c(max, min, prop)) %>%
                                   ungroup() %>%
                                   spread(key = ID ,value = val)})) %>%
                               mutate(dist = map(data, function(xx){xx %>% dplyr::select(-roi) %>% vegdist(method = "bray")})) %>%
                               mutate(cluster = map2(dist, data,
                                                     function(xx,yy){
                                                       h <- hclust(xx, method = "ward.D2")
                                                       d <- as.dendrogram(h)
                                                       dd <- dendro_data(d, type = "rectangle")
                                                       
                                                       clust <- 
                                                         cutree(h, k = 6) %>% as_tibble() %>%
                                                         rename(cluster = "value") %>%
                                                         mutate(label = as.character(row_number()),
                                                                cluster = as.factor(cluster))
                                                       
                                                       label <- 
                                                         yy %>% dplyr::select(roi) %>% mutate(label = as.character(row_number())) %>%
                                                         left_join(., roiTable %>% dplyr::select(Name, Annotation, label.mtb) %>% rename(roi = "Name"), by = "roi")
                                                       dd$labels <-
                                                         dd$labels %>%
                                                         left_join(., label, by = "label") %>%
                                                         left_join(., clust, by = "label") 
                                                       return(dd)
                                                     })) %>%
                               mutate(plot.cluster = map(cluster,
                                                         function(xx){
                                                           ggplot(segment(xx)) +
                                                             geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
                                                             geom_text(data = xx$labels, aes(x, y, label = Annotation, color = cluster), hjust = 1, angle = 90) +
                                                             theme_bw() +
                                                             ylim(-1,3.5) +
                                                             theme_void()
                                                         })) %>%
                               mutate(plot.dlink.cluster = pmap(list(data, filter, cluster),
                                                                function(xx, yy, zz){
                                                                  bind.tmp <- 
                                                                    Basic_info %>% 
                                                                    dplyr::select(ID, filter, River, D.link_Shreve, distance.from.ocean) %>%
                                                                    filter(filter == yy)
                                                                  res <-
                                                                    xx %>%
                                                                    gather(-roi, key = "ID", value = "val") %>%
                                                                    left_join(., bind.tmp, by = "ID") %>%
                                                                    left_join(., as_tibble(zz$labels), by ="roi") %>%
                                                                    arrange(cluster) %>%
                                                                    nest(-roi, -label.mtb) %>%
                                                                    mutate(roi = factor(roi, levels = .$roi),
                                                                           label.mtb = factor(label.mtb, levels = .$label.mtb)) %>%
                                                                    unnest(data) %>%
                                                                    ggplot(aes(x = D.link_Shreve, y = val, color = River, group = roi)) +
                                                                    geom_rect(aes(fill = cluster),xmin = -Inf,xmax = Inf,
                                                                              ymin = -Inf,ymax = Inf,alpha = 0.01) +
                                                                    geom_point() +
                                                                    geom_smooth(se = F)  +
                                                                    facet_wrap(label.mtb ~ .) +
                                                                    theme_bw() +
                                                                    scale_x_continuous(breaks = c(0, 20, 40)) +
                                                                    scale_y_continuous(breaks = c(0, 0.5, 1))
                                                                }))
                           }))
  

permanova.res.nmr.tmp <- list()
for(i in 1:nrow(DF_nmr.2)){
  data.tmp <- DF_nmr.2[i,] %>% dplyr::select(data) %>% unnest() %>% left_join(., Basic_info %>% dplyr::select(ID, filter, River), by = c("ID", "filter"))
  TMP <- data.tmp  %>% select(-c(ID, filter, River)) %>% as.data.frame()
  TMP2 <- data.tmp  %>% select(c(ID, filter, River)) %>% as.data.frame()
  res <- pairwise.adonis2(TMP ~  River * filter, data = TMP2)
  permanova.res.nmr.tmp <- append(permanova.res.nmr.tmp, list(res))
}
DF_nmr.3 <-
  DF_nmr.2 %>%
  bind_cols(., tibble(pairwise.permanova = permanova.res.nmr.tmp)) %>%
  mutate(pairwise.permanova = map(pairwise.permanova,
                                  function(x){
                                    tibble(pair = names(x),
                                           result = x) %>%
                                      filter(pair != "parent_call") %>%
                                      mutate(result = map(result,
                                                          function(x){
                                                            as_tibble(x, rownames = "factor")
                                                          }))
                                  })) 

  
df_plot.pairwise.permanova <-
  bind_rows(DF_microbe.3[8, 13], DF_nmr.3[1, 15])  %>%
  bind_cols(tibble(data = c("asv", "nmr")),.) %>%
  mutate(plot = map(pairwise.permanova,
                    function(x){
                      x %>%
                        unnest() %>%
                        filter(factor == "River") %>%
                        separate(pair, into = c("a", "b"), sep = "_vs_") %>%
                        mutate(pair = map2(a,b, 
                                           function(x,y){
                                             order <- c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream")
                                             append(x,y) %>% factor(., levels = order) %>% sort() %>% as.character() })) %>%
                        mutate(a = map_chr(pair, ~.[1]), b = map_chr(pair, ~.[2])) %>%
                        left_join(crossing(a = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream"), 
                                           b = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream")),
                                  .,
                                  by = c("a", "b")) %>%
                        mutate(a = factor(a, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream")),
                               b = factor(b, levels = rev(c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream")))) %>%
                        mutate(label = round(R2, 2) %>% as.character()) %>%
                        ggplot(aes(x = a, y =b, color = R2, label = label)) +
                        geom_point(aes(size = R2)) +
                        geom_text(color = "black") +
                        scale_color_gradient(low = "white", high = "red", limits = c(0, NA)) +
                        scale_size(limits = c(0, NA), range = c(0, 15)) +
                        theme_bw() +
                        labs(x = "", y = "") +
                        theme(legend.position = "none")
                      
                    })) 


final_graph_microbiome <-
  DF_microbe.2[[5,11]][[1]] + labs(title = "(A) Microbiome composition at family level") + 
  DF_microbe.2[[5,12]][[1]] + 
  DF_microbe.2[[8,9]][[1]] + labs(title = "(B) nMDS at ASV level")  + 
  plot_layout(width = c(1.5, 0.4, 0.5))

final_graph_metabolome <-
  DF_nmr.2[[1,9]][[1]] + labs(title = "(A) Metabolome composition for ROIs") + 
  DF_nmr.2[[1,10]][[1]] + 
  DF_nmr.2[[1,8]][[1]] + labs(title = "(B) nMDS for ROIs")  + 
  plot_layout(width = c(1.5, 0.4, 0.5))

final.bar <-
  ((DF_microbe.2[[5,11]][[1]] + labs(title = "(A) Microbiome composition at family level")) / (DF_nmr.2[[1,9]][[1]] + labs(title = "(C) Metabolome composition for ROIs")))
final.bar.label <-
  ((DF_microbe.2[[5,12]][[1]]) / (DF_nmr.2[[1,10]][[1]]))
final.nMDS <-
  (DF_microbe.2[[8,9]][[1]] + labs(title = "(B) nMDS at ASV level")) / ( DF_nmr.2[[1,8]][[1]] + labs(title = "(D) nMDS for ROIs"))

ggsave(plot = final_graph_microbiome, file = str_c(SAVE_DIR_FIG, "/final.graph.microbiome.pdf"), width = 15, height = 6)
ggsave(plot = final_graph_metabolome, file = str_c(SAVE_DIR_FIG, "/final.graph.metabolome.pdf"), width = 15, height = 6)
ggsave(plot = final.bar, file = str_c(SAVE_DIR_FIG, "/final.bar.png"), width = 10, height = 10)
ggsave(plot = final.bar.label, file = str_c(SAVE_DIR_FIG, "/final.bar.label.png"), width = 2, height = 10)
ggsave(plot = final.nMDS, file = str_c(SAVE_DIR_FIG, "/final.nMDS.png"), width = 4.5, height = 10)

for(i in 1:nrow(DF_microbe.2)){
  plot.tmp <-
    DF_microbe.2[[i,11]][[1]] + #labs(title = "(A) Microbiome composition at family level") + 
    DF_microbe.2[[i,12]][[1]] + 
    plot_layout(width = c(1.5, 0.4))
  ggsave(plot = plot.tmp, file = str_c(SAVE_DIR_FIG, "/bar_", DF_microbe.2[[i,1]], ".png"),
         width = 12, height = 6)
}


permanova.res.table <-
  bind_rows(DF_microbe.2[[8,10]][[1]] [[1]] %>% as_tibble(rownames = "factor") %>% mutate(data = "metabolome"),
            DF_nmr.2[[1,11]][[1]] [[1]] %>% as_tibble(rownames = "factor") %>% mutate(data = "microbiome")) %>%
  filter(factor != "Total") %>%
  dplyr::select(data, factor, R2, `Pr(>F)`) %>%
  mutate(a = str_c(data, factor, sep = "_")) %>%
  mutate(p = ifelse(`Pr(>F)` < 0.001, "***", ifelse(`Pr(>F)` < 0.01, "**", ifelse(`Pr(>F)` < 0.05, "*", ifelse(`Pr(>F)` %>% is.na(), "", ""))))) %>%
  mutate(p = ifelse(is.na(p), "", p)) %>%
  mutate(R2_p = str_c(round(R2, 2), p)) %>%
  dplyr::select(data, factor, R2_p) %>%
  spread(key = factor, value = R2_p) %>%
  select(data, filter.tmp, River.tmp, `filter.tmp:River.tmp` ,Residuals) %>%
  set_names("Dataset", "Filter", "Tributary", "Filter × Tributary", "Residuals")

pairwise.permanova.res.table <-
  bind_rows(DF_nmr.3[[1,15]][[1]] %>% mutate(data = "metabolome"),
          DF_microbe.3[[8,13]][[1]] %>% mutate(data = "microbiome"))  %>%
  mutate(pair = map(pair,
                    function(x){
                      str_split(x, pattern = "_vs_") %>% .[[1]] %>% 
                        factor(., levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream")) %>% 
                        sort() %>%
                        as.character()
                    })) %>%
  mutate(a = map_chr(pair, ~.[1]), b = map_chr(pair, ~.[2])) %>%
  mutate(a = factor(a, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream")),
         b = factor(b, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream"))) %>%
  arrange(data, a, b) %>%
  dplyr::select(-pair) %>%
  unnest() %>%
  dplyr::select(data, a, b, factor, R2, `Pr(>F)` ) %>%
  filter( factor != "Total") %>%
  mutate(p = ifelse(`Pr(>F)` < 0.001, "***", ifelse(`Pr(>F)` < 0.01, "**", ifelse(`Pr(>F)` < 0.05, "*", "")))) %>%
  mutate(p = ifelse(is.na(p), "", p)) %>%
  mutate(R2_p = str_c(round(R2, 2), p)) %>%
  dplyr::select(data, a, b, factor, R2_p) %>%
  spread(key = factor, value = R2_p) %>%
  mutate(pair = str_c(a, b, sep = " v.s. ")) %>%
  dplyr::select(data, pair, filter, River, `River:filter` , Residual) %>%
  set_names("Dataset", "Pair", "Filter", "Tributary", "Filter × Tributary", "Residuals")

write_excel_csv(permanova.res.table, str_c(SAVE_DIR, "/permanova.csv"))
write_excel_csv(pairwise.permanova.res.table, str_c(SAVE_DIR, "/pairwise.permanova.csv"))

#########################
### Network analysis
#########################
Basic_info
data_list$DF_water
data_list$DF_ICP
data_list$DF_nmr
data_list$DF_microbe
data_list$DF_picrust

Basic_info_bind <-
  Basic_info %>% 
  dplyr::select(-c(time, latitude, longitude, distance, euclidean.distance)) %>%
  mutate(spring.water = ifelse(isTRUE(spring.water), 1, 0))
  
#  mutate(date = as.factor(date), River = factor(River, levels = c("Tsurumi", "Onda", "Hayabuchi", "Yagami", "Main stream", "Pacific ocean"))) 
DF_water_bind <-
  data_list$DF_water
DF_ICP_bind <-
  data_list$DF_ICP %>%
  mutate_at(vars(-ID), ~ifelse(. < 0, 0, .))
DF_nmr_bind <-
  data_list$DF_nmr %>%
  filter(method == "proportion") %>%
  dplyr::select(data) %>%
  unnest() %>%
  set_names(str_c("NMR_", names(.))) %>%
  rename(ID = "NMR_ID", filter = "NMR_filter")
DF_microbe_bind <-
  data_list$DF_microbe %>%
  filter(category == "f") %>%
  dplyr::select(data3) %>%
  unnest() %>%
  gather(-c(ID, filter), key = "taxa", value = "prop") %>%
  nest(-taxa) %>%
  mutate(max.prop = map_dbl(data, ~max(.$prop, na.rm = T))) %>%
  filter(max.prop > 0.01) %>%
  dplyr::select(-max.prop) %>%
  arrange(taxa) %>%
  unnest() %>%
  spread(key = taxa, value = prop) %>%
  dplyr::select(-Unassigned)

DF_pathway_bind <-
  data_list$DF_picrust %>% 
  filter(method == "data_pathway.2") %>% 
  dplyr::select(data) %>%
  unnest(data) %>%
  gather(-c(ID, filter), key = "path", value = "prop") %>%
  nest(-path) %>%
  #  mutate(max.prop = map_dbl(data, ~max(.$prop, na.rm = T))) %>%
  #  filter(max.prop > 0.01) %>%
  #  dplyr::select(-max.prop) %>%
  arrange(path) %>%
  unnest() %>%
  spread(key = path, value = prop) 
  
########################################
# iCAMP
########################################
icamp.out <- read_rds(str_c(SAVE_DIR, "/icamp.out.rds"))

df.eco.process <- 
  icamp.out[1] %>% .[[1]] %>% as_tibble() %>%
  gather(-c(sample1, sample2), key = "Ecological.Process", value = "Importance") %>%
  mutate(Ecological.Process = factor(Ecological.Process, 
                                     levels = c("Heterogeneous.Selection", "Homogeneous.Selection", "Dispersal.Limitation", "Homogenizing.Dispersal", "Drift.and.Others"))) %>%
  separate(sample1, into = c("from", "from_filter"), sep = "_") %>%
  separate(sample2, into = c("to", "to_filter"), sep = "_") %>%
  mutate(from = as.factor(as.double(from)),
         to = as.factor(as.double(to))) 

niche_diff <-
  Basic_info_bind %>% dplyr::select(-spring.water) %>%
  left_join(., DF_water_bind, by = "ID") %>%
  left_join(., DF_ICP_bind, by = "ID") %>%
  left_join(., DF_nmr_bind, by = c("ID", "filter")) %>%
  #left_join(., DF_microbe_bind, by = c("ID", "filter"))  %>%
  dplyr::select(-date, -ID_for_arrange,) %>%
  gather(-c(ID, filter, River), key = "param", value = "val") %>%
  nest(-param) %>% 
  mutate(crossing = map(data, 
                        function(x){
                          x1 <- x %>% rename(from = "ID", from_filter = "filter", from_River = "River", val1 = "val")
                          x2 <- x %>% rename(to = "ID", to_filter = "filter", to_River = "River", val2 = "val")
                          res <- crossing(x1, x2) %>% mutate(diff = abs(val1 - val2)) %>% dplyr::select(-c(val1, val2))
                        })) %>%
  dplyr::select(-data)

river.distance_diff <-
  left_join(niche_diff[[1,2]][[1]] %>% dplyr::select(-diff), 
            distance_each %>% dplyr::select(-River) , by = c("from", "to")) %>%
  rename(diff = "distance") %>%
  nest(.key = "crossing") %>%
  mutate(param = "river.distance") %>%
  dplyr::select(param, crossing)

euclidean.distance_diff <-
  left_join(niche_diff[[1,2]][[1]] %>% dplyr::select(-diff), 
            euclidean.distance_each, by = c("from", "to")) %>%
  rename(diff = "euclidean.distance") %>%
  nest(.key = "crossing") %>%
  mutate(param = "euclidean.distance") %>%
  dplyr::select(param, crossing)

library(lmerTest)

diff.list <-
  bind_rows(river.distance_diff, euclidean.distance_diff, niche_diff) %>%
  mutate(df.eco.process_for.graph = map(crossing,
                                        function(x){
                                          df.eco.process %>%
                                            full_join(., x, by = c("from",  "to", "from_filter", "to_filter"))%>%
                                            mutate(filter.pair = ifelse(from_filter == "0.2um" & to_filter == "0.2um", "FL",
                                                                        ifelse(from_filter == "1um" & to_filter == "1um", "PA", "FL-PA"))) %>%
                                            filter(filter.pair != "FL-PA") %>%
                                            mutate(filter.pair = factor(filter.pair, levels = c("PA", "FL"))) %>%
                                            filter(!is.na(Importance)) %>%
                                            mutate(river.pair = ifelse(from_River == "Main stream" | to_River == "Main stream", "with downstream", "within upstream")) %>%
                                            mutate(from_River.2 = ifelse(from_River %in% c("Tsurumi", "Onda"), "upstream.1",
                                                                         ifelse(from_River %in% c("Hayabuchi", "Yagami"), "upstream.2", "downstream"))) %>%
                                            mutate(to_River.2 = ifelse(to_River %in% c("Tsurumi", "Onda"), "upstream.1",
                                                                       ifelse(to_River %in% c("Hayabuchi", "Yagami"), "upstream.2", "downstream"))) %>%
                                            mutate(river.pair.2 = map2_chr(from_River.2, to_River.2,
                                                                           function(x,y){
                                                                             append(as.character(x), as.character(y)) %>%
                                                                               sort() %>%
                                                                               str_c(., collapse = " & ") 
                                                                           })) %>%
                                            mutate(river.pair.3 = ifelse(river.pair == "with mainstream", "without mainstream", 
                                                                         ifelse(from_River == to_River, "within upstream subbasin", "across upstream subbasin"))) %>%
                                            mutate(Ecological.Process = ifelse(Ecological.Process == "Heterogeneous.Selection", "HeS",
                                                                               ifelse(Ecological.Process == "Homogeneous.Selection", "HoS",
                                                                                      ifelse(Ecological.Process == "Dispersal.Limitation", "DL",
                                                                                             ifelse(Ecological.Process == "Homogenizing.Dispersal", "HD", "DR"))))) %>%
                                            mutate(Ecological.Process = factor(Ecological.Process, levels = c("HeS", "HoS", "DL", "HD", "DR")))
                                        })) %>%
  mutate(plot_eco.process = map2(df.eco.process_for.graph, param,
                                 function(x,y){
                                   x  %>%
                                     ggplot(aes(x = diff, y = Importance, color = river.pair)) +
                                     geom_point(alpha = 0.03) +
                                     facet_grid(Ecological.Process ~ filter.pair) + 
                                     geom_smooth(se = F, method = "lm", alpha = 0.6) +
                                     #scale_x_log10() +
                                     theme_bw() +
                                     labs(title = y)
                                 })) %>%
  mutate(plot_eco.process.log = map(plot_eco.process,
                                 function(x,y){
                                   x + scale_x_log10() 
                                 })) %>%
  mutate(plot_ecoprocess_boxplot = map2(df.eco.process_for.graph, param,
                                        function(x, y){
                                          x %>%
                                            ggplot(aes(x = Ecological.Process, y = Importance, color = river.pair)) +
                                            geom_boxplot() +
                                            facet_grid(filter.pair ~ .) + 
                                            #geom_smooth(se = F, method = "lm") +
                                            #scale_x_log10() +
                                            theme_bw() 
                                        })) %>%
  mutate(lm.each = map(df.eco.process_for.graph,
                       function(x){
                         x %>%
                           mutate(diff.log = log10(diff)) %>%
                           nest(-c(filter.pair, Ecological.Process)) %>%
                           mutate(lm = map(data, function(x){lm(Importance ~ diff, data = x) %>% summary()})) %>%
                           mutate(lm.r.squared = map_dbl(lm, function(x){x$r.squared})) %>%
                           mutate(lm.table = map(lm, function(x){x$coefficients %>% as_tibble(rownames = "lm.param") %>% rename(p.val =`Pr(>|t|)`)})) %>%
                           mutate(lm.log = map(data, function(x){lm(Importance ~ diff.log, data = x %>% filter(!is.infinite(diff.log))) %>% summary()})) %>%
                           mutate(lm.r.squared.log = map_dbl(lm.log, function(x){x$r.squared})) %>%
                           mutate(lm.table.log = map(lm.log, function(x){x$coefficients %>% as_tibble(rownames = "lm.param") %>% rename(p.val =`Pr(>|t|)`)})) %>%
                           mutate(ancova = map(data, 
                                               function(x){
                                                 model <- lm(Importance ~ diff * river.pair, data = x)
                                                 res <- anova(model) %>% as_tibble(rownames = "param") %>%
                                                   mutate(sig = ifelse(`Pr(>F)`<0.001, "***", ifelse(`Pr(>F)`<0.01, "**", ifelse(`Pr(>F)`<0.05, "*", NA))))
                                               }))
                       })) %>%
  mutate(lm.each.table = map(lm.each,
                             function(x){
                               x %>%
                                 dplyr::select(Ecological.Process, filter.pair, lm.r.squared, lm.table) %>%
                                 unnest()
                             })) %>%
  mutate(lm.log.each.table = map(lm.each,
                             function(x){
                               x %>%
                                 dplyr::select(Ecological.Process, filter.pair, lm.r.squared.log, lm.table.log) %>%
                                 unnest()
                             })) %>%
  mutate(ancova.res = map(lm.each,
                          function(x){
                            x %>%
                              dplyr::select(Ecological.Process, filter.pair, ancova) %>%
                              unnest()
                          }))

library(ggsignif)
plot_ep.comp <-
  diff.list[[1,3]][[1]] %>%
  ggplot(aes(x = Ecological.Process, y = Importance, color = filter.pair)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.7, 
                                             jitter.width = 0.3), alpha=0.05) +
  geom_boxplot(outlier.color = NA) +
  theme_bw() +
  labs(x = "Ecological Process", y = "Importance", color = "Filter") +
  geom_signif(y_position=c(0.35, 0.8, 0.7, 0.2, 0.72), 
              xmin=c(0.8, 1.8, 2.8, 3.8, 4.8), 
              xmax=c(1.2, 2.2, 3.2, 4.2, 5.2), 
              annotation=c("***", "***", "***", "***", "***"), color = "black") +
  ylim(0, 0.85) +
  theme(legend.position = "right")

plot_ep.comp.2 <-
  diff.list[[1,3]][[1]] %>%
  mutate(St.De = ifelse(Ecological.Process %in% c("HeS", "HoS"), "Deterministic", "Stochastic")) %>%
  nest(-from, -to, -St.De, -filter.pair) %>%
  mutate(Importance = map_dbl(data, function(x){x$Importance %>% sum()})) %>%
  ggplot(aes(x = St.De, y = Importance, color = filter.pair)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.7, 
                                             jitter.width = 0.3), alpha=0.05) +
  geom_boxplot(outlier.color = NA) +
  theme_bw() +
  labs(x = "Ecological Process", y = "Importance", color = "Filter") +
  geom_signif(y_position=c(0.8, 1), 
              xmin=c(0.8, 1.8), 
              xmax=c(1.2, 2.2), 
              annotation=c("***", "***"), color = "black") +
  ylim(0, 1.05) +
  theme(legend.position = "right") +
  theme(legend.position = "none")

ep_lm.res <- 
  diff.list[[1,3]][[1]] %>%
  nest(-Ecological.Process, - filter.pair) %>% 
  mutate(lm.res = map_chr(data,
                      function(x){
                        lm(Importance ~ diff * river.pair, data = x) %>% summary() %>%
                          .$coefficients %>%
                          as_tibble(rownames = "factor") %>%
                          bind_cols(tibble(factor2 = c("I", "D", "T", "D×T")), .) %>%
                          mutate(lab = ifelse(`Pr(>|t|)` < 0.001, "***", ifelse(`Pr(>|t|)` < 0.01, "**", ifelse(`Pr(>|t|)` < 0.05, "*", NA)))) %>%
                          filter(factor2 != "I") %>%
                          #filter(!is.na(lab)) %>%
                          mutate(lab = ifelse(is.na(lab), "", lab)) %>%
                          mutate(lab2 = str_c(factor2, lab, sep = " ")) %>%
                          .$lab2 %>%
                          str_c(.,collapse = "\n")
                      })) %>%
  dplyr::select(-data) 

plot_ep.dist <-
  ggplot() +
  geom_point(data = diff.list[[1,3]][[1]], aes(x = diff, y = Importance, color = river.pair), alpha = 0.05) +
  geom_smooth(data = diff.list[[1,3]][[1]], aes(x = diff, y = Importance, color = river.pair), method = "lm", se = F) +
  facet_grid(filter.pair ~ Ecological.Process) +
  geom_text(data = ep_lm.res, aes(x = 1, y = 0.6, label = lm.res), size = 3, hjust = 0) +
  theme_bw() +
  labs(title = "", x = "Distance (km)", color = "Tributary pair") +
  theme(plot.title = element_blank())

plot_ep <- 
  (plot_ep.comp.2 + plot_ep.comp) / plot_ep.dist + 
  plot_layout(nrow = 2, heights = c(1,2))  
ggsave(plot = plot_ep,
       file = str_c(SAVE_DIR_FIG, "/plot_ep.pdf"),
       height = 6, width = 12)

SAVE_DIR_FIG_EP <- str_c(SAVE_DIR_FIG, "/ep", sep = "")
SAVE_DIR_FIG_EPLOG <- str_c(SAVE_DIR_FIG, "/ep_log", sep = "")
if(file.exists(SAVE_DIR_FIG_EP) == FALSE){dir.create(SAVE_DIR_FIG_EP)}
if(file.exists(SAVE_DIR_FIG_EPLOG) == FALSE){dir.create(SAVE_DIR_FIG_EPLOG)}


for(i in 1:nrow(diff.list)){
  df.tmp <-  diff.list %>% .[i,]
  param.tmp <- df.tmp$param
  plot.tmp1 <- df.tmp %>% dplyr::select(plot_eco.process) %>% .[[1,1]] %>% .[[1]]
  plot.tmp2 <- df.tmp %>% dplyr::select(plot_eco.process.log) %>% .[[1,1]] %>% .[[1]]
  ggsave(plot = plot.tmp1, file = str_c(SAVE_DIR_FIG_EP, "/", param.tmp, ".png"), width = 10, height = 10)
  ggsave(plot = plot.tmp2, file = str_c(SAVE_DIR_FIG_EPLOG, "/", param.tmp, ".png"), width = 10, height = 10)
}

diff.list.save <-
  diff.list %>%
  dplyr::select(param, crossing, df.eco.process_for.graph, lm.each) 

write_rds(diff.list.save, str_c(SAVE_DIR, "/diff.list.rds"), compress = "gz")
diff.list <- read_rds(str_c(SAVE_DIR, "/diff.list.rds"))


########################################
# cluster analysis
########################################
Basic_info_bind.2 <-
  Basic_info_bind %>%
  filter(filter == "1um") %>%
  dplyr::select(-c(date, filter, River))
DF_water_bind.2 <-
  DF_water_bind
DF_ICP_bind.2 <-
  DF_ICP_bind 
DF_nmr_bind.2 <-
  DF_nmr_bind %>%
  gather(-c(ID, filter), key = "taxa", value = "prop") %>%
  mutate(taxa2 = str_c(filter, taxa, sep = "___")) %>%
  dplyr::select(-c(filter, taxa)) %>%
  spread(key = taxa2, value = prop)
DF_microbe_bind.2 <-
  DF_microbe_bind %>%
  gather(-c(ID, filter), key = "taxa", value = "prop") %>%
  mutate(taxa2 = str_c(filter, taxa, sep = "___")) %>%
  dplyr::select(-c(filter, taxa)) %>%
  spread(key = taxa2, value = prop)




Bind_data_cluster <-
  Basic_info_bind.2 %>%
  left_join(., DF_water_bind.2, by = "ID") %>%
  left_join(., DF_ICP_bind.2, by = "ID") %>%
  left_join(., DF_nmr_bind.2, by = "ID") %>%
  left_join(., DF_microbe_bind.2, by = "ID") 
  
distance <- 
  Bind_data_cluster %>% 
  dplyr::select(-ID, -ID_for_arrange) %>% 
  vegdist(na.rm = T) 
hc <- 
  hclust(distance, "ward.D2")
dhc <- 
  as.dendrogram(hc)
ddata <- 
  dendro_data(dhc, type = "rectangle")

ddata$labels <-
  ddata$labels %>%
  left_join(., 
            Basic_info_bind %>% 
              filter(filter == "1um") %>% 
              dplyr::select(ID, River, D.link_Shreve, distance.from.ocean) %>%
              mutate(distance.from.ocean = round(distance.from.ocean, 1)) %>%
              rename(label = "ID"),
            by = "label")

plot.dendro <-
  ggplot() +
  geom_segment(data = segment(ddata), 
               aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_point(data = label(ddata), 
             aes(x = x, y = y -0.05, size = D.link_Shreve, color = River), alpha = 0.7) +
  geom_text(data = label(ddata), 
             aes(x = x, y = y - 0.1, label = distance.from.ocean), 
             size = 3, angle = 45, hjust = 3) +
#  coord_flip() +
#  scale_y_reverse(expand = c(0.05, 0))+
  scale_color_manual(values = c("Tsurumi" = hue_pal()(5)[1], 
                                "Onda" = hue_pal()(5)[2], 
                                "Hayabuchi" = hue_pal()(5)[3], 
                                "Yagami" = hue_pal()(5)[4], 
                                "Main stream" = hue_pal()(5)[5]#, 
                                #"Pacific ocean" = "grey40"
                                )) +
  theme_void()

#############################

Bind_data <-
  Basic_info_bind %>% dplyr::select(-spring.water) %>%
  left_join(., DF_water_bind, by = "ID") %>%
  left_join(., DF_ICP_bind, by = "ID") %>%
  left_join(., DF_nmr_bind, by = c("ID", "filter")) %>%
  left_join(., DF_microbe_bind, by = c("ID", "filter")) %>%
  dplyr::select(-ID, -ID_for_arrange) %>%
  mutate(filter = as.factor(filter))
  
# summarize water quality data
library(ggrepel)
wq_pca <- 
  Bind_data %>%
  filter(filter == "1um") %>%
  select_if(colnames(.) %in% c("River", "D.link_Shreve") | str_detect(colnames(.), "ICP") | str_detect(colnames(.), "WQ")) %>%
  dplyr::select(-WQ_water.temperature) %>%
  nest() %>%
  mutate(pca = map(data,
                   function(x){
                     x[-c(1,2)] %>% prcomp(scale = T) %>% summary()
                   })) %>%
  mutate(plot_pca = map2(data, pca,
                         function(x,y){
                           y$x %>%
                             as_tibble() %>%
                             bind_cols(x,.) %>%
                             ggplot(aes(x = PC1, y = PC2, color = River, size = D.link_Shreve)) +
                             geom_point(alpha = 0.6) +
                             theme_bw() +
                             labs(x = str_c("PC1 (", round(y$importance[2,1]*100, 1),"%)"),
                                  y = str_c("PC2 (", round(y$importance[2,2]*100, 1),"%)"),
                                  color = "Tributary", size = "Shreve index")
                         })) %>%
  mutate(plot_loading = map(pca,
                            function(x){
                              library(ggarchery)
                              df.tmp <- 
                                t(t(x$rotation) * x$sdev) %>%
                                as_tibble(rownames = "param") %>%
                                mutate(param = str_split(param, pattern = "_")) %>%
                                mutate(param = map_chr(param, function(x){x[2]})) %>%
                                mutate(param = ifelse(param == "Cd", "EC", param))
                              
                              circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
                                r = diameter / 2
                                tt <- seq(0,2*pi,length.out = npoints)
                                xx <- center[1] + r * cos(tt)
                                yy <- center[2] + r * sin(tt)
                                return(data.frame(x = xx, y = yy))
                              }
                              
                              df.circle <- circleFun(c(0,0),2,npoints = 100)
                              
                              ggplot() +
                                geom_path(data = df.circle, aes(x, y)) + 
                                geom_arrowsegment(data = df.tmp, aes(x = 0, xend = PC1,
                                                                     y = 0, yend = PC2),
                                                  size = 2, color = hue_pal()(1), alpha = 0.6,
                                                  arrows = list(arrow(angle = 25, type = "open"))) +
                                geom_label_repel(data = df.tmp, aes(x = PC1, y = PC2, label = param), max.overlaps = 100) +
                                theme_bw() +
                                labs(x = str_c("PC1 (", round(x$importance[2,1]*100, 1),"%)"),
                                     y = str_c("PC2 (", round(x$importance[2,2]*100, 1),"%)"))
                              
                            })) %>%
  mutate(plot_fin = map2(plot_pca, plot_loading, function(x,y){x/y}))

ggsave(plot = wq_pca[[1,5]][[1]],
       file = str_c(SAVE_DIR_FIG, "/wq_pca.png"),
       width = 6, height = 9)

library(arules)
library(ggforce)
cor.graph <-
  Bind_data %>%
  nest(-filter) %>%
  mutate(data2 = map(data,
                     function(x){
                       x %>%
                         dplyr::select(-date) %>%
                         mutate(River_tsurumi = ifelse(River == "Tsurumi", TRUE, FALSE),
                                River_onda = ifelse(River == "Onda", TRUE, FALSE),
                                River_hayabuchi = ifelse(River == "Hayabuchi", TRUE, FALSE),
                                River_yagami = ifelse(River == "Yagami", TRUE, FALSE),
                                River_mainstream = ifelse(River == "Main stream", TRUE, FALSE)) %>%
                         dplyr::select(-River) %>%
                         mutate(id = row_number()) %>%
                         gather(-c(id, River_tsurumi, River_onda, River_hayabuchi, River_yagami, River_mainstream), key = "param", value = "val") %>%
                         nest(-param) %>%
                         mutate(q3 = map_dbl(data, function(x){quantile(x$val) %>% .[[4]]})) %>%
                         unnest() %>%
                         mutate(val2 = ifelse(val > q3, TRUE, FALSE)) %>%
                         dplyr::select(-c(val, q3)) %>%
                         spread(key = param, value = val2) %>%
                         dplyr::select(-id) %>%
                         select_if(function(x){length(unique(x)) > 1}) 
                     })) %>%
  mutate(transaction = map(data2, function(x){as(x, "transactions")})) %>%
  mutate(rules = map(transaction, function(x){apriori(x, parameter=list(support=0.063,confidence=0.25,maxlen=2))})) %>%
  mutate(rules_lis = map(rules, function(x){capture.output(inspect(x))})) %>%
  mutate(rules_table = map(rules_lis,
                           function(x){
                             res <-
                               x[-1]  %>%
                               as_tibble() %>%
                               mutate(value = str_replace_all(value, "; ", ";")) %>%
                               mutate(tmp = str_split(value, pattern = " ")) %>%
                               mutate(col.list = map(tmp, function(x){x[x != ""] %>% t()}))  %>% 
                               dplyr::select(col.list) %>%
                               unnest(col.list) %>%
                               .[[1]] %>%
                               as_tibble() %>%
                               set_names(c("id", "source", "direction", "target", "support", "confidence", "converge", "lift", "count")) %>%
                               dplyr::select(-direction, -id) %>%
                               type_convert() %>%
                               mutate(G.source = ifelse(str_detect(source, "d__"), "Microbiome",
                                                        ifelse(str_detect(source, "NMR_"), "Metabolome",
                                                               ifelse(str_detect(source, "ICP_") | str_detect(source, "WQ_") , "Water quality", "Field information"))),
                                      G.target = ifelse(str_detect(target, "d__"), "Microbiome",
                                                        ifelse(str_detect(target, "NMR_"), "Metabolome",
                                                               ifelse(str_detect(target, "ICP_") | str_detect(target, "WQ_") , "Water quality", "Field information")))) %>% 
                               filter(!(source == "{}" | target == "{}")) %>%
                               arrange(G.source, G.target, source, target)
                           })) %>%
  mutate(plot.arules = map(rules_table,
                           function(x){
                             x %>%
                               mutate_at(vars(source, target), ~str_sub(., 2, -2)) %>%
                               filter(G.source != G.target) %>%
                               nest() %>%
                               mutate(q.lift = map(data, ~quantile(.$lift, probs = c(0.9)) %>% .[1])) %>%
                               unnest(data) %>%
                               filter(lift > q.lift) %>%
                               as_tbl_graph() %N>%
                               mutate(group = ifelse(str_detect(name, "WQ_"), "Water quality",
                                                     ifelse(str_detect(name, "ICP_"), "Water quality",
                                                            ifelse(str_detect(name, "NMR_"), "Metabolites",
                                                                   ifelse(str_detect(name, "d__"), "Microbes", "Field information"))))) %N>%
                               mutate(community = as.factor(group_leading_eigen())) %N>%
                               mutate(label = ifelse(group == "Field information", name, NA_character_))%N>%
                               ggraph(layout = "kk") +
                               geom_edge_link(color = "grey") +
                               geom_node_point(aes(color = group)) +
                               geom_node_text(aes(label = label), repel = T) +
                               theme_void() #+
                               #facet_nodes(community ~ .)
                           }
                           )) %>%
  mutate(tidygraph.cor = map(data,
                         function(x){
                           tmp <-
                             x %>%
                             dplyr::select(-date, -River) %>%
                             correlate(method = "spearman") %>%
                             gather(-term, key = "term2", value = "R") %>%
                             set_names("from", "to", "R") %>%
                             as_tbl_graph(directed = F) %N>%
                             mutate(group = ifelse(str_detect(name, "WQ_"), "Water quality",
                                                   ifelse(str_detect(name, "ICP_"), "Water quality",
                                                          ifelse(str_detect(name, "NMR_"), "Metabolites",
                                                                 ifelse(str_detect(name, "d__"), "Microbes", "Field information"))))) %N>%
                             mutate(label = ifelse(group == "Field information", name, NA_character_)) %N>%
                             mutate(id = row_number()) 
                           node.info <-
                             tmp %N>% 
                             as_tibble() %>%
                             dplyr::select(id, name, group)
                           tmp2 <-
                             tmp %E>%
                             left_join(., node.info %>% set_names("from", "from_name", "from_group"), by = "from") %E>%
                             left_join(., node.info %>% set_names("to", "to_name", "to_group"), by = "to") %E>%
                             mutate(edge.group = ifelse(from_group == to_group, "same", "different")) %>%
                             mutate(with.mic = ifelse(from_group == "Microbes" | to_group == "Microbes", "with microbe", "without microbe"))
                             
                           return(tmp2)
                         })) %>%
  mutate(plot.graph.cor = map(tidygraph.cor,
                          function(x){
                            tmp <- 
                              x %E>%
                              filter(!is.na(R) & abs(R) > 0.5) %>%
                              filter(from != to) %>%
                              filter(edge.group == "different") %>%
                              filter(with.mic == "with microbe")
                            node <-
                              tmp %E>%
                              as_tibble() %>%
                              dplyr::select(from, to) %>%
                              gather(key = "ft", value = "id") %>%
                              .$id %>%
                              unique() %>%
                              as_tibble() %>%
                              set_names("id") %>%
                              mutate(exist.edge = "exist")
                            tmp2 <-
                              tmp %N>%
                              left_join(., node, by = "id") %>%
                              mutate(label2 = ifelse(exist.edge == "exist", label, NA_character_)) %>%
                              mutate(label3 = ifelse(exist.edge == "exist" & group == "Water quality", name, NA_character_))
                            tmp2 %N>%
                              mutate(community = as.character(group_infomap())) %>%
                              group_by(community) %>%
                              mutate(num = n()) %>%
                              mutate(community2 = ifelse(num == 1, NA_character_, community)) %>%
                              ungroup() %>%
                              ggraph(layout = "kk") +
                              geom_edge_link(color = "grey80") +
                              geom_node_point(aes(color = group), alpha = 0.6) +
                              geom_node_text(aes(label = label3), repel = T) +
                              theme_void() +
                              geom_mark_hull(aes(x = x, y = y, group = community2))#+
                              #facet_nodes(community ~ .)
                          })) %>%
  mutate(plot.graph.cor.2 = map(data,
                                function(x){
                                  set.seed(1234)
                                  data.tmp <-
                                    x %>%
                                    dplyr::select(-date, -River, -WQ_water.temperature) %>%
                                    correlate(method = "spearman") %>%
                                    gather(-term, key = "term2", value = "R") %>%
                                    set_names("from", "to", "R") %>%
                                    left_join(., data_list$roi.table %>% dplyr::select(Name, Annotation) %>% set_names(c("from", "a_from")), by = "from") %>%
                                    left_join(., data_list$roi.table %>% dplyr::select(Name, Annotation) %>% set_names(c("to", "a_to")), by = "to") %>%
                                    mutate(from = ifelse(is.na(a_from), from, str_c("NMR_", a_from, sep = ""))) %>%
                                    mutate(to = ifelse(is.na(a_to), to, str_c("NMR_", a_to, sep = ""))) %>%
                                    dplyr::select(-c("a_from", "a_to")) %>%
                                    mutate(from.group = ifelse(str_detect(from, "WQ_"), "Water quality",
                                                               ifelse(str_detect(from, "ICP_"), "Water quality",
                                                                      ifelse(str_detect(from, "NMR_"), "Metabolites",
                                                                             ifelse(str_detect(from, "d__"), "Microbes", "Field information"))))) %>%
                                    mutate(to.group = ifelse(str_detect(to, "WQ_"), "Water quality",
                                                             ifelse(str_detect(to, "ICP_"), "Water quality",
                                                                    ifelse(str_detect(to, "NMR_"), "Metabolites",
                                                                           ifelse(str_detect(to, "d__"), "Microbes", "Field information"))))) %>%
                                    #filter(abs(R) > 0.5 & from.group != to.group) %>%
                                    filter(abs(R) > 0.5 & from.group != to.group & (from.group == "Microbes" | to.group == "Microbes")) %>%
                                    mutate(slope = ifelse(R > 0, "+", "-")) %>%
                                    as_tbl_graph() %N>%
                                    mutate(group = ifelse(str_detect(name, "WQ_"), "Water quality",
                                                          ifelse(str_detect(name, "ICP_"), "Water quality",
                                                                 ifelse(str_detect(name, "NMR_"), "Metabolites",
                                                                        ifelse(str_detect(name, "d__"), "Microbes", "Field information"))))) %N>%
                                    mutate(label = ifelse(group == "Water quality", name, NA_character_)) %N>%
                                    mutate(label2 = str_split(label, pattern = "_"),
                                           label2 = map_chr(label2, ~.[length(.)]),
                                           label2 = ifelse(label2 == "Cd", "EC", label2),
                                           label2 = ifelse(name == "D.link_Shreve", "Shreve.index", ifelse(name == "distance.from.ocean", "distance.to.estuary", label2))) %N>% 
                                    mutate(label3 = ifelse(group == "Microbes", str_split(name, pattern = "__"), NA_character_)) %N>%
                                    mutate(label3 = map_chr(label3, ~.[length(.)])) %N>%
                                    mutate() %>%
                                    mutate(community = as.factor(group_infomap()),
                                           degree = centrality_degree())%E>%
                                    mutate(slope = factor(slope, levels = c("+", "-"))) 
                                  
                                  set.seed(1234)
                                  plot.1 <-
                                    data.tmp %>%
                                    ggraph(layout = "fr") +
                                    geom_mark_hull(aes(x = x, y = y, group = community, fill = community), 
                                                   concavity = 4, expand = unit(2.5, "mm"), radius = unit(2.5, "mm"), 
                                                   color = NA) +
                                    geom_edge_link(color = "grey80", width = 1.5, aes(linetype = slope)) +
                                    scale_edge_linetype_manual(values = c("solid","dotted")) +
                                    geom_node_point(aes(color = group, size = centrality_degree()), alpha = 0.6) +
                                    geom_node_text(aes(label = label2), repel = T) +
                                    theme_void() +
                                    theme(panel.border = element_rect(colour = "grey50", fill=NA, size=1))
                                  
                                  set.seed(1234)
                                  plot.2 <-
                                    data.tmp %N>%
                                    filter(community %in% c(1,2)) %>%
                                    ggraph(layout = "fr") +
                                    geom_mark_hull(aes(x = x, y = y, group = community, fill = community), 
                                                   concavity = 4, expand = unit(2.5, "mm"), radius = unit(2.5, "mm"), 
                                                   color = NA) +
                                    scale_fill_manual(values = hue_pal()(12)[c(1,2)]) +
                                    geom_edge_link(color = "grey80", width = 1.5, aes(linetype = slope)) +
                                    scale_edge_linetype_manual(values = c("solid","dotted")) +
                                    geom_node_point(aes(color = group, size = centrality_degree()), alpha = 0.6) +
                                    geom_node_text(aes(label = label3), repel = T) +
                                    theme_void() +
                                    theme(panel.border = element_rect(colour = "grey50", fill=NA, size=1))
                                  
                                  res <- list(data.tmp, plot.1, plot.2)
                                  return(res)
                                })) %>%
  mutate(index = map(plot.graph.cor.2,
                     function(x){
                       df.node <- x[[1]] %N>%mutate(modularity = graph_modularity(group=as.factor(community)))%>% as_tibble()
                       df.edge <- x[[1]] %E>% as_tibble()
                       res <- 
                         tibble(num.node = nrow(df.node),
                                num.edge = nrow(df.edge)/2,
                                mean.degree = mean(df.node$degree),
                                modurality = df.node$modularity %>% .[1])
                     })) 

plot_network_final<-
  (cor.graph[[1,11]][[1]][[2]] + geom_text(x = -10, y = Inf, label = "A", vjust = "inward", hjust = "inward")+ theme(legend.position = "none") + 
     cor.graph[[1,11]][[1]][[3]] + theme(legend.position = "none")) /
  (cor.graph[[2,11]][[1]][[2]] + theme(legend.position = "none") + 
     cor.graph[[2,11]][[1]][[3]] + guides(color=guide_legend("Group"), fill = "none", size = "none")) 

  
ggsave(plot = plot_network_final,
       file = str_c(SAVE_DIR_FIG, "/final.network.pdf"),
       width = 12, height = 12)

cor.graph %>%
  mutate(nodes = map2(plot.graph.cor.2, filter,
                      function(x, y){
                        x[[1]] %N>%
                          as_tibble() %>%
                          mutate(filter = y)
                      })) %>%
  dplyr::select(nodes) %>%
  unnest() %>%
  group_by(filter, group) %>%
  summarize(nodes = n()) %>%
  ggplot(aes(x = group, y = nodes, fill = filter)) +
  geom_bar(stat = "identity", position = "dodge")


cor.graph %>%
  mutate(edges = map2(plot.graph.cor.2, filter,
                      function(x, y){
                        x[[1]] %E>%
                          as_tibble() %>%
                          mutate(filter = y)
                      })) %>%
  dplyr::select(edges) %>%
  unnest() %>%
  group_by(from.group, to.group, filter) %>%
  summarize(n_edges = n()) %>%
  ggplot(aes(x = from.group, y = n_edges, fill = filter)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(to.group ~ .)


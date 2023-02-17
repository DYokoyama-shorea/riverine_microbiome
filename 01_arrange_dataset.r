rm(list =ls())

library(tidyverse)
library(readxl)
library(corrr)
library(tidygraph)
library(ggraph)
library(fs)
library(lubridate)

CWD <- getwd()
DATA_DIR <- str_c(CWD, "/data")
SAVE_DIR <- str_c(CWD, "/save")
if(file.exists(SAVE_DIR) == FALSE){
  dir.create(SAVE_DIR)
}


DF_info.org <-
  read_xlsx(str_c(DATA_DIR, "/Tsurumi_river_Sampling.xlsx"), sheet = "Sheet1", col_types = "text") %>%
  mutate(ID = as.double(ID) %>% as.factor(),
         ID_for_arrange = as.double(ID_for_arrange) %>% as.factor(),
         date = ymd(date),
         latitude = as.double(latitude),
         longitude = as.double(longitude))
DF_water.org <-
  read_xlsx(str_c(DATA_DIR, "/tsurumi.river_water.xlsx"), sheet = "Sheet1")
DF_ICP.org <-
  read_xlsx(str_c(DATA_DIR, "/ICP.xlsx"), sheet = "per.wavelength_selected_2")
DF_microbe.org <-
  read_rds(str_c(DATA_DIR, "/microbe_tsurumi.river_rarefied.rds"))
#  read_rds("Tsurumi.river.microbe.rds")
DF_picrust.org <-
  read_rds(str_c(DATA_DIR, "/DF_picrust.prokatlas_tsurumi.river.rds")) %>%
  filter(method != "data_prokatlas.2")
DF_prokatlas.org <-
  read_rds(str_c(DATA_DIR, "/DF_picrust.prokatlas_tsurumi.river.rds")) %>%
  filter(method == "data_prokatlas.2")
prokatlas_group <-
  read_delim(str_c(DATA_DIR, "/prokatlas_group.txt"), delim = "\t")

# load nmr dat
nmr.info <- 
  read_csv(str_c(DATA_DIR,"/nmr/sample_info_2dj.csv")) %>%
  mutate(date.mes = ymd(date.mes),
         filter = as.factor(filter),
         ID = as.factor(ID),
         File = str_c(File, ".ucsf")) %>%
  filter(final_data == "final") 
roi.table <-
  read_csv(str_c(DATA_DIR, "/nmr/roiTable_tsurumi.river.csv", sep = ""))

nmr.ls <- dir_ls(str_c(DATA_DIR,"/nmr"), regexp = "roiSummary.txt")
DF_nmr.org <-
  as_tibble()
for(i in 1:length(nmr.ls)){
  date.mes <- 
    nmr.ls[i] %>% 
    str_split(pattern = "/") %>% unlist() %>% .[length(.)] %>% 
    str_split(pattern = "_") %>% unlist() %>% .[1] %>%
    ymd()
  tmp <- 
    read_delim(nmr.ls[i], delim = "\t") %>%
    mutate(date.mes = date.mes) %>%
    dplyr::select(File, date.mes, everything())
  DF_nmr.org <-
    bind_rows(DF_nmr.org, tmp)
}

DSS_ROI <-
  roi.table %>%
  filter(Annotation == "DSS") %>%
  .$Name
DF_nmr <-
  nmr.info %>%
  left_join(., DF_nmr.org, by = c("date.mes", "File")) %>%
  rename(DSS = DSS_ROI) %>%
  dplyr::select(-c(date.mes, File, final_data)) %>%
  mutate_at(vars(-c(ID, filter, DSS)), ~./DSS) %>%
  dplyr::select(-DSS) # delete DSS
DF_nmr.2 <-
  DF_nmr %>%
  nest() %>%
  mutate(data2 = map(data,
                     function(x){
                       x %>%
                         mutate(sum = rowSums(x %>% dplyr::select(-c(ID, filter)))) %>%
                         mutate_at(vars(-c(ID, filter)), ~./sum) %>%
                         dplyr::select(-sum)
                     }))  %>%
  set_names(c("raw", "proportion")) %>% 
  gather(key = "method", value = "data")


DF_info <-
  bind_rows(DF_info.org %>% mutate(filter = "1um"), 
            DF_info.org %>% mutate(filter = "0.2um"))

DF_water <-
  DF_water.org %>%
  set_names(str_c("WQ_", names(.))) %>%
  rename(ID = "WQ_ID")%>%
  mutate(ID = as.factor(ID))

DF_ICP <-
  DF_ICP.org %>%
  set_names(str_c("ICP_", names(.))) %>%
  rename(ID = "ICP_ID") %>%
  mutate(ID = as.factor(ID)) %>%
  mutate_at(vars(-ID), ~ifelse(. < 0, 0, .))

DF_microbe <-
  DF_microbe.org %>%
  mutate(data3 = map(data3,
                     function(x){
                       tmp1 <-
                         x %>%
                         dplyr::select(Description) 
                       tmp2 <-
                         x %>%
                         dplyr::select(-Description) %>%
                         dplyr::select_if(~sum(.) != 0)
                       bind_cols(tmp1, tmp2) %>%
                         separate(Description, sep = "_", into = c("tsurumi", "ID", "filter")) %>%
                         dplyr::select(-tsurumi) %>%
                         mutate(ID = as.double(ID) %>% as.factor())
                     }))


DF_prokatlas <-
  DF_prokatlas.org[[1,2]][[1]] %>%
  rename(X1 = "ID") %>%
  left_join(., prokatlas_group, by = "X1") %>%
  group_by(Description, category_1) %>%
  summarize(prop = sum(prop, na.rm = T)) %>%
  separate(Description, into = c("a", "ID", "filter"), sep = "_") %>%
  filter(!(ID %in% c(55, 56, 57))) %>%
  filter(!is.na(ID)) %>%
  mutate(ID = as.double(ID) %>% as.factor()) %>%
  dplyr::select(-a) %>%
  ungroup()

DF_picrust <-
  DF_picrust.org %>%
  mutate(data = map(data, 
                    function(x){
                      x %>%
                        filter(!is.na(Description)) %>%
                        spread(key = ID, value = prop) %>%
                        separate(Description, into = c("project", "ID", "filter"), sep = "_") %>%
                        mutate(ID = as.double(ID) %>% as.factor()) %>%
                        filter(!(ID %in% c(55, 56, 57))) %>%
                        dplyr::select(-project)
                      
                    })) 



data_list <-
  list(DF_info = DF_info, 
       DF_water = DF_water, 
       DF_ICP = DF_ICP, 
       DF_microbe = DF_microbe,
       DF_nmr = DF_nmr.2, 
       roi.table = roi.table,
       DF_prokatlas = DF_prokatlas,
       DF_picrust = DF_picrust)

write_rds(data_list, str_c(SAVE_DIR, "/data_list.rds", sep = ""))





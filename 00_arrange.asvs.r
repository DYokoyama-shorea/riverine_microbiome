rm(list =ls())

####################################################################################
###
### CWD --- dir1 --- export
###     |        |-- pathway_export
###     |        |-- ko_export
###     |        |-- ec_export
###     |        |-- prokatlas
###     |
###     |-- dir2 --- export   
###     |        |-- pathway_export
###     |        |-- ko_export
###     |        |-- ec_export
###     |        |-- prokatlas
###     |
###     |-- dir3 
###
###
####################################################################################


### parameter
min.read <- 5000   # minimum total read per sample for filtering samples with low quality  

###
library(readxl)
library(tidyverse)
library(fs)
library(vegan)

CWD <-
  getwd()

DATA_DIR <- str_c(CWD, "/data")

DF_orig <-
  as_tibble_col(str_c(DATA_DIR, "/miseq_out"), column_name = "dir") %>%
  mutate(info = map(dir,
                    function(x){
                      read_csv(str_c(x, "/Miseq_Info.csv")) 
                    })) %>%
  mutate(data_asv = map2(dir, info, 
                        function(x, y){
                          col.ls <- y$`#SampleID`
                          read_tsv(str_c(x, "/export/feature-table.tsv", sep = ""), skip = 1) %>%
                            dplyr::select(`#OTU ID`, one_of(col.ls))
                        })) %>%
  mutate(taxonomy = map(dir, 
                        function(x){
                          read_tsv(str_c(x, "/export/taxonomy.tsv", sep = ""))
                        })) %>%
  mutate(data_pathway = map2(dir, info,  
                            function(x, y){
                              col.ls <- y$`#SampleID`
                              read_tsv(str_c(x, "/pathway_export/feature-table.tsv", sep = ""), skip = 1) %>%
                                dplyr::select(`#OTU ID`, one_of(col.ls))
                            }))  %>%
  mutate(data_ko = map2(dir, info, 
                       function(x, y){
                         col.ls <- y$`#SampleID`
                         read_tsv(str_c(x, "/ko_export/feature-table.tsv", sep = ""), skip = 1) %>%
                           dplyr::select(`#OTU ID`, one_of(col.ls))
                       }))  %>%
  mutate(data_ec = map2(dir, info,
                       function(x, y){
                         col.ls <- y$`#SampleID`
                         read_tsv(str_c(x, "/ec_export/feature-table.tsv", sep = ""), skip = 1) %>%
                           dplyr::select(`#OTU ID`, one_of(col.ls))
                       }))  %>%
  mutate(data_prokatlas = map2(dir, info, 
                              function(x, y){
                                col.ls <- y$`#SampleID`
                                file <- dir_ls(str_c(x, "/prokatlas"), regexp = "prokatlas.txt") 
                                read_tsv(file) %>%
                                  dplyr::select(X1, one_of(col.ls))
                              }))  



DF <-
  DF_orig %>%
  mutate(sample.for.analysis = map(data_asv,
                                   function(x){
                                     colSums(x[-1]) %>%
                                       as_tibble(rownames = "sample") %>%
                                       filter(value > min.read) %>%
                                       .$sample
                                   })) %>%
  mutate(data_asv_rarefied = map(data_asv,
                                 function(x){
                                   tmp <-
                                     x %>%
                                     rename(ID = `#OTU ID`) %>% 
                                     gather(-ID, key = "sample", value = "read") %>%
                                     spread(key = ID, value = read)
                                   rarefy.limit <- 
                                     tmp %>%
                                     select(-sample) %>%
                                     rowSums() %>% 
                                     as_tibble() %>% 
                                     filter(value > min.read) %>% 
                                     arrange(value) %>% 
                                     .[[1,1]]
                                   
                                   set.seed(1234)
                                   res <-
                                     rrarefy(tmp %>% select(-sample), rarefy.limit) %>%
                                     as_tibble() %>%
                                     bind_cols(tmp %>% select(sample), .) %>%
                                     gather(-sample, key = `#OTU ID`, value = "val") %>%
                                     spread(key = sample, value = val)
                                   
                                   return(res)
                                 })) %>%
  mutate(data_asv.2 = pmap(list(data_asv, info, sample.for.analysis), 
                           function(x, y, z){
                             data.x <- x %>% rename(ID = `#OTU ID`) %>% select(one_of(c("ID", z)))
                             info <-
                               y %>%
                               rename(sample = `#SampleID`) %>%
                               select(sample, Project, Description)
                             res <-
                               data.x %>%
                               rename(Taxon = "ID") %>%
                               select(Taxon, everything()) %>%
                               gather(-Taxon, key = "sample", value = "read") %>%
                               group_by(sample) %>%
                               mutate(sum.read = sum(read)) %>%
                               ungroup() %>%
                               mutate(prop = read / sum.read) %>%
                               left_join(., info, by ="sample")
                           })) %>%
  mutate(data_asv_rarefied.2 = pmap(list(data_asv_rarefied, info, sample.for.analysis), 
                           function(x, y, z){
                             data.x <- x %>% rename(ID = `#OTU ID`) %>% select(one_of(c("ID", z)))
                             info <-
                               y %>%
                               rename(sample = `#SampleID`) %>%
                               select(sample, Project, Description)
                             res <-
                               data.x %>%
                               rename(Taxon = "ID") %>%
                               select(Taxon, everything()) %>%
                               gather(-Taxon, key = "sample", value = "read") %>%
                               group_by(sample) %>%
                               mutate(sum.read = sum(read)) %>%
                               ungroup() %>%
                               mutate(prop = read / sum.read) %>%
                               left_join(., info, by ="sample")
                           })) %>%
  mutate(data_asv_taxon = pmap(list(data_asv, taxonomy, info, sample.for.analysis),
                               function(x, y, z, a){
                                 data.x <- x %>% rename(ID = `#OTU ID`) %>% select(one_of(c("ID", a)))
                                 data.y <- y %>% rename(ID = `Feature ID`)
                                 info <-
                                   z %>%
                                   rename(sample = `#SampleID`) %>%
                                   select(sample, Project, Description)
                                 
                                 res <-
                                   data.x %>%
                                   left_join(., data.y, by = "ID") %>%
                                   select(-ID, -Confidence) %>%
                                   select(Taxon, everything()) %>%
                                   gather(-Taxon, key = "sample", value = "read") %>%
                                   filter(!str_detect(Taxon, "d__Eukaryota"))  %>%             # delete Eukayota
                                   group_by(Taxon, sample) %>%
                                   summarize(read = sum(read)) %>%
                                   ungroup() %>%
                                   group_by(sample) %>%
                                   mutate(sum.read = sum(read)) %>%
                                   ungroup() %>%
                                   mutate(prop = read / sum.read) %>%
                                   left_join(., info, by ="sample")
                                 
                               })) %>%
  mutate(data_asv_rarefied_taxon = pmap(list(data_asv_rarefied, taxonomy, info, sample.for.analysis),
                               function(x, y, z, a){
                                 data.x <- x %>% rename(ID = `#OTU ID`) %>% select(one_of(c("ID", a)))
                                 data.y <- y %>% rename(ID = `Feature ID`)
                                 info <-
                                   z %>%
                                   rename(sample = `#SampleID`) %>%
                                   select(sample, Project, Description)
                                 
                                 res <-
                                   data.x %>%
                                   left_join(., data.y, by = "ID") %>%
                                   select(-ID, -Confidence) %>%
                                   select(Taxon, everything()) %>%
                                   gather(-Taxon, key = "sample", value = "read") %>%
                                   filter(!str_detect(Taxon, "d__Eukaryota"))  %>%             # delete Eukayota
                                   group_by(Taxon, sample) %>%
                                   summarize(read = sum(read)) %>%
                                   ungroup() %>%
                                   group_by(sample) %>%
                                   mutate(sum.read = sum(read)) %>%
                                   ungroup() %>%
                                   mutate(prop = read / sum.read) %>%
                                   left_join(., info, by ="sample")
                                 
                               })) %>%
  mutate(data_pathway.2 = pmap(list(data_pathway, sample.for.analysis, info),
                               function(x, y, z){
                                 info <-
                                   z %>%
                                   rename(sample = `#SampleID`) %>%
                                   select(sample, Project, Description)
                                 tmp1 <-
                                   x %>%
                                   rename(ID = `#OTU ID`) %>%
                                   select(one_of("ID", y)) %>%
                                   gather(-ID, key = "sample", value = "read")
                                 tmp2 <-
                                   tmp1 %>%
                                   group_by(sample) %>%
                                   summarize(sum.read = sum(read))
                                 res <-
                                   left_join(tmp1, tmp2, by = "sample") %>%
                                   mutate(prop = read/ sum.read) %>%
                                   left_join(., info, by ="sample")
                                 
                                 return(res) 
                               })) %>%
  mutate(data_ko.2 = pmap(list(data_ko, sample.for.analysis, info),
                          function(x, y, z){
                            info <-
                              z %>%
                              rename(sample = `#SampleID`) %>%
                              select(sample, Project, Description)
                            tmp1 <-
                              x %>%
                              rename(ID = `#OTU ID`) %>%
                              select(one_of("ID", y)) %>%
                              gather(-ID, key = "sample", value = "read")
                            tmp2 <-
                              tmp1 %>%
                              group_by(sample) %>%
                              summarize(sum.read = sum(read))
                            res <-
                              left_join(tmp1, tmp2, by = "sample") %>%
                              mutate(prop = read/ sum.read) %>%
                              left_join(., info, by ="sample")
                            
                            return(res) 
                          })) %>%
  mutate(data_ec.2 = pmap(list(data_ec, sample.for.analysis, info),
                          function(x, y, z){
                            info <-
                              z %>%
                              rename(sample = `#SampleID`) %>%
                              select(sample, Project, Description)
                            tmp1 <-
                              x %>%
                              rename(ID = `#OTU ID`) %>%
                              select(one_of("ID", y)) %>%
                              gather(-ID, key = "sample", value = "read")
                            tmp2 <-
                              tmp1 %>%
                              group_by(sample) %>%
                              summarize(sum.read = sum(read))
                            res <-
                              left_join(tmp1, tmp2, by = "sample") %>%
                              mutate(prop = read/ sum.read) %>%
                              left_join(., info, by ="sample")
                            
                            return(res) 
                          }))  %>%
  mutate(data_prokatlas.2 = pmap(list(data_prokatlas, sample.for.analysis, info),
                                 function(x, y, z){
                                   info <-
                                     z %>%
                                     rename(sample = `#SampleID`) %>%
                                     select(sample, Project, Description)
                                   tmp1 <-
                                     x %>%
                                     rename(ID = "X1") %>%
                                     select(one_of("ID", y)) %>%
                                     gather(-ID, key = "sample", value = "read")
                                   tmp2 <-
                                     tmp1 %>%
                                     group_by(sample) %>%
                                     summarize(sum.read = sum(read))
                                   res <-
                                     left_join(tmp1, tmp2, by = "sample") %>%
                                     mutate(prop = read/ sum.read) %>%
                                     left_join(., info, by ="sample")
                                 }))


#################################################################################################################
###
###
### taxonomy
###
###
#################################################################################################################

select.col <- list("data_asv_taxon", "data_asv_rarefied_taxon")
method <- list("raw", "rarefied")

for(i in 1:length(select.col)){
  asv.table_tmp <-
    DF %>%
    select(one_of("dir", select.col[[i]])) %>%
    unnest() %>%
    select(dir, Taxon, sample, Project, Description, read, sum.read) 
  
  DF2 <-  
    tibble(category = c("d", "p", "c", "o", "f", "g", "s"),
           num = seq(1,7,1),
           data = rep(nest(asv.table_tmp)[[1]],7)) %>%
    mutate(data2 = map2(data, num,
                        function(x, y){
                          x %>%
                            mutate(taxa = str_split(Taxon, pattern = ";")) %>%
                            mutate(taxa = map(taxa, function(x){x[1:y]})) %>%
                            #                          mutate(taxa = map(taxa, function(x){x[!is.na(x)]})) %>% # to summarize unassigned higher levels
                            mutate(taxa = map_chr(taxa, function(x){str_c(x, collapse = ";")})) %>%
                            mutate(taxa = ifelse(is.na(taxa), "Unassigned", taxa)) %>%
                            group_by(taxa, dir, sample, Project, Description, sum.read) %>%
                            summarize(read = sum(read)) %>%
                            mutate(prop = read/sum.read) %>%
                            ungroup()
                        })) %>%
    mutate(data3 = map(data2,
                       function(x){
                         x %>%
                           select(-sum.read, -read) %>%
                           spread(key = taxa, value = prop) %>%
                           mutate_all(~ifelse(is.na(.), 0, .))
                       })) %>%
    mutate(data4 = map(data2,
                       function(x){
                         x %>%
                           select(-sum.read, -read) %>%
                           spread(key = taxa, value = prop) %>%
                           mutate_all(~ifelse(is.na(.), 0, .))
                       })) %>%
    select(category, data3)
  
  
  
  
  DF2_asv <-  
    DF %>% 
    select(dir, data_asv.2) %>%
    unnest() %>%
    select(dir, Taxon, sample, Project, Description, read, sum.read)  %>%
    nest() %>%
    mutate() %>%
    mutate(category = "asv", num = 8) %>%
    mutate(data2 = map2(data, num,
                        function(x, y){
                          x %>%
                            mutate(taxa = Taxon) %>%
                            select(-Taxon) %>%
                            #group_by(taxa, dir, sample, Project, Description, sum.read) #%>%
                            #summarize(read = sum(read)) %>%
                            mutate(prop = read/sum.read) %>%
                            ungroup() %>%
                            select(taxa, dir, sample, Project, Description, sum.read, read, prop)
                        })) %>%
    mutate(data3 = map(data2,
                       function(x){
                         x %>%
                           select(-sum.read, -read) %>%
                           spread(key = taxa, value = prop) #%>%
                         #mutate_all(~ifelse(is.na(.), 0, .))
                       })) %>%
    select(category, data3)
  
  
  DF3 <-
    bind_rows(DF2, DF2_asv) %>%
    mutate(num = row_number()) %>%
    dplyr::select(category, num, data3) %>%
    mutate(data3 = map(data3, function(x){x %>% dplyr::select(-c(dir, sample, Project))}))
  
  write_rds(DF3, str_c(DATA_DIR, "/microbe_tsurumi.river_", method[[i]], ".rds"))
  
}


###################################
#
# picrust and prokatlas
#
#####################################
DF4 <-
  DF %>%
  dplyr::select(data_pathway.2, data_ko.2, data_ec.2, data_prokatlas.2) %>%
  gather(key = "method", value = "data") %>%
  mutate(data = map(data,
                    function(x){
                      x %>% dplyr::select(Description, ID, prop)
                    })) 

write_rds(DF4, str_c(DATA_DIR, "/DF_picrust.prokatlas_tsurumi.river.rds"))




# load libraries ----------------------------------------------------------


# install.packages("devtools")
# devtools::install_github("ropensci/refsplitr")

library(tidyverse)
library(refsplitr)


# load clean data ---------------------------------------------------

all_pubs<-read_csv("./data_clean/all_pubs.csv") %>% 
  filter(refID != 3039) %>% 
  filter(refID != 4068) 

all_refined<-read_csv("./data_clean/all_refined.csv")

all_georef <-read_rds("./data_clean/all_georef_clean.rds")



pub_data<-all_pubs %>% select(refID,SO,PY) %>% 
  mutate(jrnl=
           case_when(
             SO == "ANIMAL BEHAVIOUR" ~ "ab",
             SO == "BEHAVIORAL ECOLOGY AND SOCIOBIOLOGY" ~ "bes",
             SO == "BEHAVIORAL ECOLOGY" ~ "be",
             .default = as.character(SO)
           )
  ) %>% 
  mutate_all(tolower) 

all_georef<-left_join(all_georef,pub_data,by="refID")

# papers 
total_pubs<-
all_georef %>% 
  select(refID) %>%
  distinct() %>% 
  tally()


# authors 

total_authors<-all_georef %>% 
  select(groupID) %>%
  distinct() %>% 
  tally()
total_authors
# authors without address (no extraction possible)
na_authors<-
all_georef %>% 
  filter(address=="could not be extracted") %>%
  select(groupID) %>%
  distinct() %>% 
  tally()
na_authors

geocoded_authors<-total_authors-na_authors

# COUNTRY (note - UK countries separate when using country.name but not when
# using country_code)
top_countries<-all_georef %>% 
  # group_by(country.name) %>% 
  group_by(country_code) %>% 
  filter(!is.na(country_code)) %>% 
  tally() %>% 
  arrange(desc(n))

# World Bank Region

all_georef %>% 
  group_by(region,jrnl) %>% 
  # group_by(region) %>% 
  filter(!is.na(region)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n)) %>% 
  arrange(jrnl,region)


# World Bank Income Category

all_georef %>% 
  # group_by(income_group,jrnl) %>% 
  group_by(income_group) %>% 
  filter(!is.na(income_group)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n))


# countries within regions

top_countries_by_region_all<-all_georef %>% 
  group_by(region,country_code) %>% 
  # group_by(region) %>% 
  filter(!is.na(region)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>%
  arrange(region,desc(n)) %>% 
  group_by(region) %>% 
  mutate(rank = row_number()) %>% 
  rename(rank_pooled=rank,
         n_pooled=n,
         perc_pooled=perc) %>% 
  relocate(rank_pooled,.before="n_pooled")



top_countries_by_region<-all_georef %>% 
  group_by(region,jrnl,country_code) %>% 
  # group_by(region) %>% 
  filter(!is.na(region)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>%
  arrange(jrnl,region,desc(n)) %>% 
  relocate(jrnl,.before=1) %>% 
  group_by(jrnl,region) %>% 
  mutate(rank = row_number()) 

ab<-top_countries_by_region %>% 
  filter(jrnl=="ab") %>% 
  pivot_wider(names_from = jrnl, values_from = n:perc, values_fill = 0) %>% 
  rename(rank_ab=rank)

be<-top_countries_by_region %>% 
  filter(jrnl=="be") %>% 
  pivot_wider(names_from = jrnl, values_from = n:perc, values_fill = 0) %>% 
  rename(rank_be=rank)

bes<-top_countries_by_region %>% 
  filter(jrnl=="bes") %>% 
  pivot_wider(names_from = jrnl, values_from = n:perc, values_fill = 0) %>% 
  rename(rank_bes=rank)

country_rankings_within_region<-top_countries_by_region_all %>% 
full_join(ab,by=c("region","country_code")) %>% 
  full_join(be,by=c("region","country_code")) %>% 
  full_join(bes,by=c("region","country_code")) 

write_csv(country_rankings_within_region,"./data_clean/country_rankings_within_region.csv")


#  
#   relocate(rank,.before="n")
  
top_countries_by_region<-top_countries_by_region %>% 
pivot_wider(names_from = jrnl, values_from = n:perc, values_fill = 0) %>% 
  relocate(perc_ab,.after="n_ab") %>% 
  relocate(perc_be,.after="n_be") %>% 
  relocate(perc_bes,.after="n_bes")

top_countries_by_region %>% 
arrange(desc(pick(starts_with("n_bes"))))

# plot points -------------------------------------------------------------



plot_addresses_points <- plot_addresses_points(all_georef)
plot_addresses_points

Plot 

plot_net_address <-plot_net_address(all_georef,
                                    lineResolution = 10,
                                    lineAlpha=.1)
plot_net_address











total_authors<-all_refined %>% 
  select(address) %>% 
  tally()
total_authors

addresses_yr<-all_refined %>% 
  select(PY) %>% 
  group_by(PY) %>% 
  tally() %>% 
  arrange(desc(PY)) %>% 
  rename(total_authors=n)
addresses_yr

addresses<-all_refined %>% 
  select(address,PY) %>% 
  group_by(address,PY) %>% 
  tally() %>% 
  filter(address=="Could not be extracted")

addresses_perc<-full_join(addresses,addresses_yr) %>% 
  mutate(perc=n/total_authors*100) %>% 
  arrange(desc(perc))
addresses_perc



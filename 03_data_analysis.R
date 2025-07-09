
# load libraries ----------------------------------------------------------


# install.packages("devtools")
# devtools::install_github("ropensci/refsplitr")

library(tidyverse)
library(refsplitr)
library(countrycode)
library(janitor)
# load clean data ---------------------------------------------------

all_pubs<-read_csv("./data_clean/all_pubs.csv") %>% 
  filter(refID != 3039) %>% 
  filter(refID != 4068) 

all_georef <-read_rds("./data_clean/all_georef_clean_2.rds") %>% 
  arrange(refID,author_order)




# add country codes, WB region and income cats ----------------------------


source("add_income_region.R")
all_georef<-add_income_region(all_georef) %>% 
  relocate(refID,.before=1)


# add pub yr, jrnl to georef ----------------------------------------------



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


# streamline income cats and regions --------------------------------------

all_georef<-all_georef %>% 
  mutate(income_group =
           case_when(
             income_group == "high income" ~ "high",
             income_group == "upper middle income" ~ "upper middle",
             income_group == "lower middle income" ~ "lower middle",
             income_group == "low income" ~ "low",
             .default = as.character(income_group)
           )
  ) %>% 
  
  mutate(region_code =
           case_when(
             region == "east asia & pacific" ~ "easp",
             region == "north america" ~ "namr",
             region == "europe & central asia" ~ "epca",
             region == "latin america & caribbean" ~ "lacb",
             region == "sub-saharan africa" ~ "suba",
             region == "south asia" ~ "soas",
             region == "middle east & north africa" ~ "mena",
             .default = as.character(region)
           )
  ) %>% 
  relocate(region_code,.after="region")


all_georef %>% select(country_name,income_group,region_code) %>% unique() %>% arrange(income_group)



# categorize as global north or global south ------------------------------


all_georef<-all_georef %>% 
  mutate(gns =
           case_when(
             income_group == "high" ~ "north",
             .default = "south"
           )
  ) %>% 
  
  mutate(gns =
           case_when(
             country_name == "trinidad tobago" ~ "south",
             country_name == "guyana" ~ "south",
             country_name == "cayman islands" ~ "south",
             country_name == "uruguay" ~ "south",
             country_name == "chile" ~ "south",
             country_name == "panama" ~ "south",
             country_name == "new caledonia" ~ "south",
             country_name == "could not be extracted" ~ NA,
              # singapore
              # brunei
              # united arab emirates 
              # qatar
              # saudi arabia
             .default = as.character(gns)
           )
  ) %>% 
  relocate(gns,.before="region") %>% 
  select(-PY.y) %>% 
  rename(PY=PY.x)



write_rds(all_georef, "./data_clean/georef_data_analysis.rds")



# missing authors ---------------------------------------------------------


authors_affils<-read_rds("./data_clean/authors_affils_df_clean.rds")

authors_affils<-authors_affils %>% 
  select(-entry_no,-source,-author_url)

authors_affils<-authors_affils %>% 
  mutate(first_middle_initials=gsub("[.]","",first_middle_initials)) %>% 
  unite("author_name",c(surname,first_middle_initials),sep=", ",remove=FALSE) 

authors_affils<-authors_affils %>% 
  group_by(author_name,author_order) %>% 
  slice_head(n=1) %>% 
  mutate(last_name=surname)
# 
# 
# missing_address<-all_georef %>% filter(is.na(lat)) %>% 
#   select(refID,PY,author_order,authorID,groupID,author_name,OI) %>% 
#   separate_wider_delim(author_name, 
#                        ", ",
#                        too_many = "merge",
#                        too_few = "align_start",
#                        names = c("last_name", "given_name"),
#                        cols_remove = FALSE) 
# 
# 
# missing_address_OI<-missing_address %>% 
#   filter(!is.na(OI))
# 
# authors_affils_OI<-authors_affils %>% 
#   filter(!is.na(OI))
# 
# missing_address_OI<-left_join(missing_address_OI,authors_affils_OI,by=c("OI","author_order","last_name")) %>% 
#   filter(!is.na(country))
# ####################
# # missing_address_OI
# ####################
# 
# missing_address<-left_join(missing_address,authors_affils,by=c("author_name","author_order"))
# 
# missing_address1<-missing_address %>% filter(!is.na(country))
# missing_address<-missing_address %>% filter(is.na(country))
# ####################
# # missingaddress1
# ####################
# missing_address<-missing_address %>% 
#   rename(first_initials=given_name.x,
#          given_name=given_name.y,
#          last_name=last_name.x) %>%
#   select(-last_name.y) 
# 
# missing_address<-left_join(missing_address,authors_affils,by="AF","author_order")
# missing_address2<-missing_address %>% filter(!is.na(country.y))
# missing_address<-missing_address %>% filter(is.na(country.y))
# ####################
# # missing_address2
# ####################
# 
# missing_address<-missing_address %>% 
#   rename(last_name=last_name.x,
#          author_order=author_order.x)
# missing_address<-left_join(missing_address,authors_affils,by="last_name","author_order")
# missing_address3<-missing_address %>% select( country,refID,author_order.x,authorID,
#                                             groupID,last_name,affil_id,
#                                             affiliation, city) %>% 
#   distinct() %>% 
#   filter(!is.na(country))
# 
# 
# 
# 
# missing_address1
# missing_address3
# 
# missing_address4<-missing_address2%>% filter(is.na(country.y)) %>% 
#   remove_empty(c("cols","rows"))
#   left_join(authors_affils,by=c("surname","author_order.x"))

# identify collaborative papers (>1 author) -------------------------------

collab_pubs<-all_georef %>% 
  group_by(refID, .drop = FALSE) %>%
  count() %>% 
  filter(n>1) %>% 
  select(refID)



all_georef_collab<-all_georef %>% 
  filter(refID %in% collab_pubs$refID)


# all coauthors same region? ----------------------------------------------


all_georef_collab<-all_georef_collab %>% 
  group_by(refID) %>% 
  # mutate(all_same_region = n_distinct(region) == 1) %>% 
  mutate(all_same_region = n_distinct(region[!is.na(region)]) == 1) %>%  # this excludes all with NA in region
  # mutate(all_same_gns = n_distinct(gns) == 1) 
  mutate(all_same_gns = n_distinct(gns[!is.na(gns)]) == 1) # this excludes all with NA in gns

# for first authors from given region, are all authors from same region?
all_georef_collab %>% 
  filter(author_order==1) %>% 
  group_by(region,all_same_region) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100)


# for first authors from GS or GN, are all authors from GS or GN?
all_georef_collab %>% 
  filter(author_order==1) %>% 
  group_by(gns,all_same_gns) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100)






# papers 
total_pubs<-
all_georef %>% 
  select(refID) %>%
  distinct() %>% 
  tally()
total_pubs

# authors 

total_authors<-all_georef %>% 
  select(groupID) %>%
  distinct() %>% 
  tally()
total_authors

# authors without address (no extraction possible)
na_authors<-
all_georef %>% 
  # filter(address=="could not be extracted") %>%
  filter(is.na(lat)) %>%
  select(groupID) %>%
  distinct() %>% 
  tally()
na_authors

geocoded_authors<-total_authors-na_authors
geocoded_authors

# nongeocoded authors


# COUNTRY (note - UK countries separate when using country.name but not when
# using country_code)
top_countries<-all_georef %>% 
  # group_by(country.name) %>% 
  group_by(country_code) %>% 
  filter(!is.na(country_code)) %>% 
  tally() %>% 
  arrange(desc(n))
top_countries



# World Bank Region

all_georef %>% 
  group_by(jrnl,region) %>% 
  # group_by(region) %>% 
  filter(!is.na(region)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n)) %>% 
  arrange(jrnl,desc(perc))


# World Bank Income Category

all_georef %>% 
  # group_by(income_group,jrnl) %>% 
  group_by(income_group) %>% 
  filter(!is.na(income_group)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n))

 # global north vs global south

all_georef %>% 
  group_by(gns) %>% 
  # filter(!is.na(region)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  arrange(desc(n)) %>% 
  arrange(gns)




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
  relocate(rank_pooled,.before="n_pooled") %>% 
  mutate(country_code=as.factor(country_code)) %>% 
  mutate(perc_pooled=round(perc_pooled,2))
top_countries_by_region_all


top_countries_by_region<-all_georef %>% 
  group_by(region,jrnl,country_code) %>% 
  # group_by(region) %>% 
  filter(!is.na(region)) %>% 
  tally() %>% 
  mutate(perc=n/sum(n)*100) %>%
  arrange(jrnl,region,desc(n)) %>% 
  relocate(jrnl,.before=1) %>% 
  group_by(jrnl,region) %>% 
  mutate(rank = row_number()) %>% 
  mutate(jrnl=as.factor(jrnl),
         country_code=as.factor(country_code)) %>% 
  mutate(perc=round(perc,2))
top_countries_by_region

ab<-top_countries_by_region %>% 
  filter(jrnl=="ab") %>% 
  pivot_wider(names_from = jrnl, values_from = n:perc, values_fill = 0) %>% 
  rename(rank_ab=rank)
ab

be<-top_countries_by_region %>% 
  filter(jrnl=="be") %>% 
  pivot_wider(names_from = jrnl, values_from = n:perc, values_fill = 0) %>% 
  rename(rank_be=rank)

bes<-top_countries_by_region %>% 
  filter(jrnl=="bes") %>% 
  pivot_wider(names_from = jrnl, values_from = n:perc, values_fill = 0) %>% 
  rename(rank_bes=rank)
bes

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
all_georef<-read_rds("./data_clean/georef_data_analysis.rds")

# ALL AUTHORS - POINTS

plot_addresses_points <- plot_addresses_points(all_georef)
# plot_addresses_points
ggsave("./img/plot_addresses_points.png",width = 40, height = 40, units = "cm")


# ALL FIRST AUTHORS - POINTS


first_author_location<-
  all_georef %>% 
  filter(author_order==1) %>% 
  filter(groupID!=31167)   # NEED TO FIX THIS LON 

plot_addresses_points_1st <- plot_addresses_points(first_author_location)
ggsave("./img/plot_addresses_points_first.png",width = 40, height = 40, units = "cm")
# LATAM 1st AUTHORS - POINTS

# papers woith first author from latin america
latam_1st<-
all_georef %>% 
  filter(author_order==1) %>% 
  filter(region=="latin america & caribbean") %>% 
  select(refID)

# LATAM 1st AUTHORS - COAUTHORS

latam_1st_location<-
  all_georef %>% 
  filter(author_order==1) %>% 
  filter(groupID!=31167) %>%  # NEED TO FIX THIS LON 
  filter(region=="latin america & caribbean") 
plot_addresses_points_latam1st <- plot_addresses_points(latam_1st_location)
# plot_addresses_points
ggsave("./img/plot_addresses_points_latam1st.png",width = 40, height = 40, units = "cm")



latam_1st<-all_georef %>% 
  filter(refID %in% latam_1st$refID) %>% 
  filter(!is.na(lat)) %>% 
  filter(!is.na(lon)) 





latam_1st<-latam_1st %>% select("authorID","university","postal_code","country"= country_name,"lat",
  "lon","groupID","author_order","address","department",
  "RP_address","RI","OI","UT","refID") 
# plot_net_address <-plot_net_address(all_georef),
plot_net_address_latam_1st <-plot_net_address(latam_1st,
                                    lineResolution = 10,
                                    lineAlpha=.05)


ggsave("./img/plot_net_address_latam_1st.png",width = 40, height = 40, units = "cm")


# USA FIRST authors


# papers with first author from latin america
usa_1st<-
  all_georef %>% 
  filter(author_order==1) %>% 
  filter(country_code=="usa") %>% 
  select(refID)

# USA 1st AUTHORS - COAUTHORS

usa_1st_location<-
  all_georef %>% 
  filter(author_order==1) %>% 
  filter(country_code=="usa") 
plot_addresses_points_usa1st <- plot_addresses_points(usa_1st_location)
# plot_addresses_points
ggsave("./img/plot_addresses_points_usa1st.png",width = 40, height = 40, units = "cm")



usa_1st<-all_georef %>% 
  filter(refID %in% usa_1st$refID) %>% 
  filter(!is.na(lat)) %>% 
  filter(!is.na(lon)) 





usa_1st<-usa_1st %>% select("authorID","university","postal_code","country"= country_name,"lat",
                                "lon","groupID","author_order","address","department",
                                "RP_address","RI","OI","UT","refID") 
# plot_net_address <-plot_net_address(all_georef),
plot_net_address_usa_1st <-plot_net_address(usa_1st,
                                              lineResolution = 10,
                                              lineAlpha=.05)


ggsave("./img/plot_net_address_usa_1st.png",width = 40, height = 40, units = "cm")








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





all_georef %>% 
  group_by(refID) %>% 
  mutate(latam=(region=="latin america & caribbean")) %>% 
  relocate(latam,.before=1)
  


all_georef %>% 
  group_by(refID) %>% 
  filter(region=="latin america & caribbean")

all_georef %>% 
  group_by(refID) %>% 
  slice_head(n=1) %>% 
  tally()
  ggplot(all_georef, aes(x=xValue, y=yValue)) +
  geom_line()
  
  
  
  
  
  
  

# how many countries per author list --------------------------------------


  
  # geo_referenced publications
  all_georef<-read_rds("./data_clean/georef_data_analysis.rds")
  
  # missing authors 
  
  authors_affils<-read_rds("./data_clean/authors_affils_df_clean.rds")
  
  authors_affils<-authors_affils %>% 
    select(-entry_no,-source,-author_url)
  
  authors_affils<-authors_affils %>% 
    mutate(first_middle_initials=gsub("[.]","",first_middle_initials)) %>% 
    unite("author_name",c(surname,first_middle_initials),sep=", ",remove=FALSE) 
  
  authors_affils<-authors_affils %>% 
    group_by(author_name,author_order) %>% 
    slice_head(n=1) %>% 
    mutate(last_name=surname)  
  
  most_common_affils<-authors_affils %>% 
    group_by(affiliation,country) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    drop_na()
  
  
  
  
  
  collab_pubs<-all_georef %>% 
    group_by(refID, .drop = FALSE) %>%
    count() %>% 
    filter(n>1) %>% 
    select(refID)
  
  
  
  all_georef_collab<-all_georef %>% 
    filter(refID %in% collab_pubs$refID)
  
  collab_1st_author_info<-all_georef_collab %>% 
    group_by(refID) %>% 
    slice_head(n=1) %>% 
    select(refID,
           country_name,
           country_code,
           gns,
           region,
           region_code,
           income_group)
  
  countries_per_article<-all_georef_collab %>% 
    group_by(refID) %>% 
    summarize(n_countries=n_distinct(country_code)) %>% 
    arrange(desc(n_countries)) 
  
  
  authors_per_article<-all_georef_collab %>% 
    group_by(refID) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    rename(n_authors=n)
  
  
  collab_1st_author_info<- left_join(collab_1st_author_info,countries_per_article,by="refID") %>% 
    left_join(authors_per_article,by="refID")
  
  collab_1st_author_info %>% 
    group_by(region) %>% 
    summarize(avg_n_authors=mean(n_authors),
              sd_n_authors=sd(n_authors),
              avg_n_countries=mean(n_countries),
              sd_n_countries=sd(n_countries)) %>% 
    drop_na()
  
  
  how many from latin america
  
  latam_pubs_total<-all_georef %>% 
    filter(region_code=="lacb") %>% 
    summarize(n_latam=n_distinct(refID))

# load libraries ----------------------------------------------------------


# install.packages("devtools")
# devtools::install_github("ropensci/refsplitr")

library(tidyverse)
library(refsplitr)
library(janitor)


# process raw data --------------------------------------------------------

all_refs<-references_read("./data_raw/wos",dir=TRUE)

write_csv(all_refs,"./data_clean/all_pubs.csv")

# need to eliminate these
all_refs<-all_refs %>% 
  filter(refID != 3039) %>% 
  filter(refID != 4068) 




# review and refine author disambiguation 


pubs_clean<-authors_clean(all_refs)
prelim<-pubs_clean$prelim
review<-pubs_clean$review

write_csv(prelim,"./data_intermediate/all_prelim.csv")
write_csv(review,"./data_intermediate/all_review.csv")


all_refined <- authors_refine(pubs_clean$review, 
                             pubs_clean$prelim)

write_csv(all_refined,"./data_clean/all_refined.csv")


# georeference the authors

all_georef <-authors_georef(data=all_refined, 
                           address_column = "address",
                           google_api=FALSE)



write_rds(all_georef,"./data_clean/all_georef.rds")

# all_georef$addresses: all info from 'refine_authors' 
# plus new columns with lat & long. It includes ALL addresses, 
# including those that could not be geocoded. 
all_georef$addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# all_georef$missing_addresses: a data frame of the addresses that 
# could NOT be geocoded.
missing_address_fix<-all_georef$missing_addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  filter(address!="Could not be extracted") %>% 
  mutate(city=word(address,-2,sep=",")) %>% 
  mutate(city=gsub(",","",city)) %>% 
  mutate(country=word(address,-1,sep=",")) %>% 
  mutate(country=gsub("[.]","",country)) %>% 
  mutate_all(trimws) %>% 
  mutate_all(tolower) %>% 
  mutate(country=
  case_when(
    country == "england" ~ "uk",
    country == "wales" ~ "uk",
    country == "northern ireland" ~ "uk",
    country == "scotland" ~ "uk",
    country == "wales" ~ "uk",
    country == "cent afr republ" ~ "central african republic",
    country == "dem rep congo" ~ "democratic republic of the congo",
    str_detect(country,"usa") ~ "usa",
    .default = as.character(country)
  )
)

missing_address_fix


country_list <- c("scotland", "england", "wales", "northern ireland","uk")

pattern <- " [a-z0-9]{3,4} [a-z0-9]{3}"



missing_address_fix <- missing_address_fix %>% 
  mutate(city=ifelse(country %in% country_list,
                                str_remove_all(city, pattern),
                                city))

pattern <- " [a-z0-9]{3} [a-z0-9]{3}"
missing_address_fix <- missing_address_fix %>% 
  mutate(city=ifelse(country %in% country_list,
                      str_remove_all(city, pattern),
                      city))
                                
                                      

pattern <- " [a-z0-9]{3,4} [a-z0-9]{3,4}"
missing_address_fix <- missing_address_fix %>% 
  mutate(city=ifelse(country %in% country_list,
                      str_remove_all(city, pattern),
                      city))



missing_address_fix <- missing_address_fix %>% 
  mutate(country=ifelse(city=="winston-salem",
                     "usa",
                     country))
country_list<-c("usa")
pattern <- "[a-z]{1,3}-[0-9a-z]{3,6}"

missing_address_fix <- missing_address_fix %>% 
  mutate(city=ifelse(country %in% country_list,
                     city,
                     str_remove_all(city, pattern)))





pattern <- "[a-z]{1,3}-[0-9]{3,5}"

missing_address_fix <- missing_address_fix %>% 
  mutate(city=str_remove_all(city, pattern))




pattern <- "[0-9]{2,7}"

missing_address_fix <- missing_address_fix %>% 
  mutate(city=str_remove_all(city, pattern))


missing_address_fix<-missing_address_fix %>% 
  mutate_all(trimws) %>% 
  select(-n) %>% 
  unite("addr",
        city:country,
        sep=",",
        remove=FALSE)



missing_to_geocode<-missing_address_fix %>% 
  select(addr) %>% 
  distinct()

missing_to_geocode <- tidygeocoder::geocode(missing_to_geocode, addr, 
                                      method = "osm", lat = latitude, long = longitude)


missing_address_fix<-missing_address_fix %>% 
  left_join(missing_to_geocode,by="addr")

missing_to_geocode_country<-missing_address_fix %>% 
  filter(is.na(latitude)) %>% 
  select(country) %>% 
  distinct()

missing_to_geocode_country<-
  tidygeocoder::geocode(missing_to_geocode_country, country, 
                        method = "osm", lat = latitude, long = longitude)


missing_address_fix<-left_join(missing_address_fix,
                               missing_to_geocode_country,
                               by="country")

missing_address_fix<-missing_address_fix %>% 
  mutate(latitude.x=if_else(is.na(latitude.x),latitude.y,latitude.x)) %>% 
  mutate(longitude.x=if_else(is.na(longitude.x),longitude.y,longitude.x)) %>% 
  select(-longitude.y,
         -latitude.y) %>% 
  rename(longitude=longitude.x,
         latitude=latitude.x) 
  
all<-all_georef$addresses

all<-all %>% 
  mutate_all(tolower)


all<-left_join(all,missing_address_fix,by="address")

all<-all %>% 
  mutate(latitude=as.character(latitude),
         longitude=as.character(longitude)) %>% 
  mutate(city.x=if_else(country.y=="uk",
                        city.y,
                        city.x))%>% 
  mutate(city_check=city.x==city.y) %>% 
  mutate(city.x=if_else(city_check==FALSE,
                        city.y,
                        city.x)) %>% 
  mutate(country.x=if_else((city_check==TRUE | is.na(city_check)),
                        country.x,
                        country.y)) %>% 
  mutate(lat=if_else(is.na(lat),
                           latitude,
                           lat)) %>% 
  mutate(lon=if_else(is.na(lon),
                     longitude,
                     lon)) %>% 
  select(-city.y,
         -country.y,
         -latitude,
         -longitude,
         -city_check) %>% 
  rename(city=city.x,
         country=country.x)
  

write_rds(all,"./data_clean/all_georef_clean.rds")



source("add_income_region.r")
all_georef<-add_income_region(all_georef)

write_rds(all,"./data_clean/all_georef_clean.rds")

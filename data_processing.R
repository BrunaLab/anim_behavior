library(tidyverse)
library(refsplitr)

refs<-references_read("./wos",dir=TRUE)
write_csv(refs,"./data_clean/ab_pubs.csv")
# ab_pubs<-read_csv("./data_clean/ab_pubs.csv")

refs<-refs %>% 
  filter(refID != 3039) %>% 
  filter(refID != 4068) 

ab_pubs_clean<-authors_clean(refs)
ab_prelim<-ab_pubs_clean$prelim
ab_review<-ab_pubs_clean$review

write_csv(prelim,"./data_clean/ab_prelim.csv")
write_csv(review,"./data_clean/ab_review.csv")


ab_refined <- authors_refine(ab_pubs_clean$review, 
                             ab_pubs_clean$prelim)

write.csv(ab_refined,"./data_clean/ab_refined.csv")



ab_georef <-authors_georef(data=ab_refined, 
                           address_column = "address",
                           google_api=FALSE)



write_rds(ab_georef,"./data_clean/ab_georef.rds")

# ab_georef$addresses: all info from 'refine_authors' 
# plus new columns with lat & long. It includes ALL addresses, 
# including those that could not be geocoded. 
ab_georef$addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# ab_georef$missing_addresses: a data frame of the addresses that 
# could NOT be geocoded.
ab_georef$missing_addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# ab_georef$no_missing_addresses: a data frame with ONLY the addresses
# that were geocoded. 

# foo<-ab_georef$no_missing_addresses %>% slice(1:100)
plot_addresses_points <- plot_addresses_points(ab_georef$no_missing_addresses)
plot_addresses_points


plot_net_address <-plot_net_address(ab_georef$no_missing_addresses,
                                    lineResolution = 10,
                                    lineAlpha=.1)
plot_net_address





total_authors<-ab_refined %>% 
  select(address) %>% 
  tally()
total_authors

addresses_yr<-ab_refined %>% 
  select(PY) %>% 
  group_by(PY) %>% 
  tally() %>% 
  arrange(desc(PY)) %>% 
  rename(total_authors=n)
addresses_yr

addresses<-ab_refined %>% 
  select(address,PY) %>% 
  group_by(address,PY) %>% 
  tally() %>% 
  filter(address=="Could not be extracted")

addresses_perc<-full_join(addresses,addresses_yr) %>% 
  mutate(perc=n/total_authors*100) %>% 
  arrange(desc(perc))
addresses_perc



library(countrycode)

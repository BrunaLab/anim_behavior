library(tidyverse)
library(refsplitr)

# install.packages("devtools")
devtools::install_github("ropensci/refsplitr")
# animal behavior ---------------------------------------------------------


refs_ab<-references_read("./data_raw/ab",dir=TRUE)
write_csv(refs,"./data_clean/ab_pubs.csv")
# ab_pubs<-read_csv("./data_clean/ab_pubs.csv")

refs<-refs %>% 
  filter(refID != 3039) %>% 
  filter(refID != 4068) 

ab_pubs_clean<-authors_clean(refs)
ab_prelim<-ab_pubs_clean$prelim
ab_review<-ab_pubs_clean$review

write_csv(prelim,"./data_intermediate/ab_prelim.csv")
write_csv(review,"./data_intermediate/ab_review.csv")


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

# behavioral ecology ------------------------------------------------------



refs_be<-references_read("./data_raw/be",dir=TRUE)
write_csv(refs_be,"./data_clean/be_pubs.csv")
# be_pubs<-read_csv("./data_clean/be_pubs.csv")
# 
# refs<-refs %>% 
#   filter(refID != 3039) %>% 
#   filter(refID != 4068) 

be_pubs_clean<-authors_clean(refs_be)
be_prelim<-be_pubs_clean$prelim
be_review<-be_pubs_clean$review

write_csv(be_prelim,"./data_intermediate/be_prelim.csv")
write_csv(be_review,"./data_intermediate/be_review.csv")


be_refined <- authors_refine(be_pubs_clean$review, 
                             be_pubs_clean$prelim)

write.csv(be_refined,"./data_clean/be_refined.csv")



be_georef <-authors_georef(data=be_refined, 
                           address_column = "address",
                           google_api=FALSE)



write_rds(be_georef,"./data_clean/be_georef.rds")

# be_georef$addresses: all info from 'refine_authors' 
# plus new columns with lat & long. It includes ALL addresses, 
# including those that could not be geocoded. 
be_georef$addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# be_georef$missing_addresses: a data frame of the addresses that 
# could NOT be geocoded.
be_georef$missing_addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# be_georef$no_missing_addresses: a data frame with ONLY the addresses
# that were geocoded. 

# foo<-be_georef$no_missing_addresses %>% slice(1:100)
plot_addresses_points <- plot_addresses_points(be_georef$no_missing_addresses)
plot_addresses_points


plot_net_address <-plot_net_address(be_georef$no_missing_addresses,
                                    lineResolution = 10,
                                    lineAlpha=.1)
plot_net_address





total_authors<-be_refined %>% 
  select(address) %>% 
  tally()
total_authors

addresses_yr<-be_refined %>% 
  select(PY) %>% 
  group_by(PY) %>% 
  tally() %>% 
  arrange(desc(PY)) %>% 
  rename(total_authors=n)
addresses_yr

addresses<-be_refined %>% 
  select(address,PY) %>% 
  group_by(address,PY) %>% 
  tally() %>% 
  filter(address=="Could not be extracted")

addresses_perc<-full_join(addresses,addresses_yr) %>% 
  mutate(perc=n/total_authors*100) %>% 
  arrange(desc(perc))
addresses_perc



library(countrycode)


# behav ecology & sociobiology --------------------------------------------





refs_bes<-references_read("./data_raw/bes",dir=TRUE)
write_csv(refs_bes,"./data_clean/bes_pubs.csv")
# bes_pubs<-read_csv("./data_clean/bes_pubs.csv")
# 
# refs<-refs %>% 
#   filter(refID != 3039) %>% 
#   filter(refID != 4068) 

bes_pubs_clean<-authors_clean(refs_bes)
bes_prelim<-bes_pubs_clean$prelim
bes_review<-bes_pubs_clean$review

write_csv(bes_prelim,"./data_intermediate/bes_prelim.csv")
write_csv(bes_review,"./data_intermediate/bes_review.csv")


bes_refined <- authors_refine(bes_pubs_clean$review, 
                             bes_pubs_clean$prelim)

write.csv(bes_refined,"./data_clean/bes_refined.csv")



bes_georef <-authors_georef(data=bes_refined, 
                           address_column = "address",
                           google_api=FALSE)



write_rds(bes_georef,"./data_clean/bes_georef.rds")

# bes_georef$addresses: all info from 'refine_authors' 
# plus new columns with lat & long. It includes ALL addresses, 
# including those that could not be geocoded. 
bes_georef$addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# bes_georef$missing_addresses: a data frame of the addresses that 
# could NOT be geocoded.
bes_georef$missing_addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# bes_georef$no_missing_addresses: a data frame with ONLY the addresses
# that were geocoded. 

# foo<-bes_georef$no_missing_addresses %>% slice(1:100)
plot_addresses_points <- plot_addresses_points(bes_georef$no_missing_addresses)
plot_addresses_points


plot_net_address <-plot_net_address(bes_georef$no_missing_addresses,
                                    lineResolution = 10,
                                    lineAlpha=.1)
plot_net_address





total_authors<-bes_refined %>% 
  select(address) %>% 
  tally()
total_authors

addresses_yr<-bes_refined %>% 
  select(PY) %>% 
  group_by(PY) %>% 
  tally() %>% 
  arrange(desc(PY)) %>% 
  rename(total_authors=n)
addresses_yr

addresses<-bes_refined %>% 
  select(address,PY) %>% 
  group_by(address,PY) %>% 
  tally() %>% 
  filter(address=="Could not be extracted")

addresses_perc<-full_join(addresses,addresses_yr) %>% 
  mutate(perc=n/total_authors*100) %>% 
  arrange(desc(perc))
addresses_perc



library(countrycode)

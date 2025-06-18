
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

all_georef <-read_rds("./data_clean/all_georef.rds")



# all_georef$addresses: all info from 'refine_authors' 
# plus new columns with lat & long. It includes ALL addresses, 
# including those that could not be geocoded. 
all_georef$addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# all_georef$missing_addresses: a data frame of the addresses that 
# could NOT be geocoded.
all_georef$missing_addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# all_georef$no_missing_addresses: a data frame with ONLY the addresses
# that were geocoded. 

# foo<-all_georef$no_missing_addresses %>% slice(1:100)

plot_addresses_points <- plot_addresses_points(all_georef$no_missing_addresses)
plot_addresses_points


plot_net_address <-plot_net_address(all_georef$no_missing_addresses,
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



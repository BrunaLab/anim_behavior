
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
all_georef$missing_addresses %>% 
  group_by(address) %>% 
  tally() %>% 
  arrange(desc(n))

# all_georef$no_missing_addresses: a data frame with ONLY the addresses
# that were geocoded. 



library(rscopus)
library(tidyverse)
library(refsplitr)
library(countrycode)
library(janitor)

# load clean data ---------------------------------------------------

all_pubs<-read_csv("./data_clean/all_pubs.csv") %>% 
  filter(refID != 3039) %>% 
  filter(refID != 4068) 

all_georef <-read_rds("./data_clean/all_georef_clean.rds") %>% 
  arrange(refID,author_order)


missing_add<-all_georef %>% 
  filter(address=="could not be extracted") %>% 
  select(refID)

search_term_all<-all_pubs %>% 
  filter(refID %in% missing_add$refID) %>% 
  select(refID,DI,SO)

# loop ---------------------------------------------------------------
  

search_term<-search_term_all %>% slice(1:2000)
part<-1
search_term<-search_term_all %>% slice(2001:7292)
part<-2
search_term<-search_term_all %>% slice(7293:11284)
part<-3
# search_term<-search_term_all %>% slice(6001:8000)
# part<-4
# search_term<-search_term_all %>% slice(8001:9497)
# part<-5
# search_term<-search_term_all %>% slice(9498:11284)
# part<-6

  # https://www.scopus.com/pages/organization/60024266
  



# Initialize empty data frames
all_papers <- data.frame()
all_affiliations <- data.frame()
all_authors <- data.frame()



  term <- seq_along(search_term$DI)
  
  for (h in term){
    
      
    query_string<-paste("(DOI(",search_term[h,2],")"," AND SRCTITLE(",search_term[h,3],"))",sep="")
      
    
      # api1: 38c1ea28aed25f40f11034d20557ccde
      # 8d8d7b628fae6e1a5a04db969b0bca93
      # api2: 8e204bc721cb41c0251c8846351342b0
      # api3: c253aa47dd592442b1d5ad7ded7b0514
      # api4: 8d8d7b628fae6e1a5a04db969b0bca93
      
      # c253aa47dd592442b1d5ad7ded7b0514 throttled 5/16
      # 38c1ea28aed25f40f11034d20557ccde throttled 6/24
      scopus_data <- rscopus::scopus_search(query_string,
                                            max_count=8000,
                                            view = "COMPLETE",
                                            api_key = "8d8d7b628fae6e1a5a04db969b0bca93")
      
      
      
      
      scopus_data_raw <- gen_entries_to_df(scopus_data$entries)
      scopus_data_raw_df<-scopus_data_raw$df
      scopus_data_raw_df$refID<-as.numeric(search_term[h,1])
      
      scopus_data_raw_affil<-scopus_data_raw$affiliation
      scopus_data_raw_affil$refID<-as.numeric(search_term[h,1])
      
      scopus_data_raw_author<-scopus_data_raw$author
      scopus_data_raw_author$refID<-as.numeric(search_term[h,1])
      # scopus_data_raw$index<-h
      if(nrow(scopus_data_raw$df)==1 & ncol(scopus_data_raw$df)==3){
        next
      }else{
        
        # Append to master data frames
        
        all_papers <- bind_rows(all_papers, scopus_data_raw_df)
        all_affiliations <- bind_rows(all_affiliations, scopus_data_raw_affil)
        all_authors <- bind_rows(all_authors, scopus_data_raw_author)
      }
  }
  
  
  write_csv(all_papers,  paste("./data_raw/scopus_api/papers/all_papers_",part,".csv",sep=""))
  write_csv(all_affiliations, paste("./data_raw/scopus_api/affils/all_affiliations_",part,".csv",sep=""))
  write_csv(all_authors, paste("./data_raw/scopus_api/authors/all_authors_",part,".csv",sep=""))
  


# bind CSVs in each folder ------------------------------------------------

  
  
  library(fs)
  # #####################################
  data_dir<-"./data_raw/scopus_api"
  dir = TRUE
  include_all=FALSE
  # #####################################
  
  
  # Define folder paths
  
  folder_path_papers  <- file.path(data_dir, "papers")
  folder_path_authors <- file.path(data_dir, "authors")
  folder_path_affils  <- file.path(data_dir, "affils")
  
  # Validate structure
  validate_csv_structure <- function(...) {
    folders <- list(...)
    file_counts <- map_int(folders, ~ length(list.files(.x, pattern = "\\.csv$", full.names = TRUE)))
    
    if (any(file_counts == 0) || length(unique(file_counts)) != 1) {
      stop("ERROR: Either the file path is incorrect OR folders contain non-CSV or mismatched files.")
    }
    
    message("Now processing all references files")
  }
  validate_csv_structure(folder_path_papers)
  validate_csv_structure(folder_path_authors)
  validate_csv_structure(folder_path_affils)
  validate_csv_structure(folder_path_papers, folder_path_authors, folder_path_affils)
  
  
  # bind the csv's together -------------------------------------------------
  
  
  
  # fs::dir_ls(data_dir)
  # binder using fs package wrapper for purrr
  # https://www.gerkelab.com/blog/2018/09/import-directory-csv-purrr-readr/
  
  # affil binder -----------------------------------------------------------
  
  data_dir_affils<-"./data_raw/scopus_api/affils"
  
  csv_files_affils_all <- fs::dir_ls(data_dir_affils, regexp = "\\.csv$")
  
  
  cvs_binder_affils <- function(csv_files_affils) {
    
    affils_df <- csv_files_affils %>%
      map_dfr(~ read_csv(.x),
              .id = "source")
    
  }
  
  affils_df_1<-cvs_binder_affils(csv_files_affils_all)
  write_csv(affils_df_1,"./data_raw/scopus_api/affils_df.csv")
  
  # author binder -----------------------------------------------------------
  
  data_dir_authors<-"./data_raw/scopus_api/authors"
  csv_files_authors_all <- fs::dir_ls(data_dir_authors, regexp = "\\.csv$")
  
  
  cvs_binder_authors <- function(csv_files_authors) {
    
    authors_df <- csv_files_authors %>%
      map_dfr(~ read_csv(.x),
              .id = "source")
    
  }
  
  authors_df_1<-cvs_binder_authors(csv_files_authors_all)
  write_csv(authors_df_1,"./data_raw/scopus_api/authors_df.csv")
  
  # papers binder -----------------------------------------------------------
  
  data_dir_papers<-"./data_raw/scopus_api/papers"
  csv_files_papers_all <- fs::dir_ls(data_dir_papers, regexp = "\\.csv$")
  
  cvs_binder_papers <- function(csv_files_papers) {
    
    papers_df <- csv_files_papers %>%
      map_dfr(~ {
        df <- read_csv(.x) 
        
        # Ensure consistent column names across files
        required_columns <- c("prism:eIssn", "prism:issn", "prism:coverDate") # Add any other necessary columns
        missing_cols <- setdiff(required_columns, names(df))
        
        # Add missing columns as NA
        if (length(missing_cols) > 0) {
          df[missing_cols] <- NA
        }
        
        # Convert all columns to character type BEFORE returning
        df <- df %>% 
          mutate_all(as.character)
        
        return(df)  
      }, .id = "source")
    
    return(papers_df)
  }
  
  
  
  
  
  papers_df_1<-cvs_binder_papers(csv_files_papers_all)
  write_csv(papers_df_1,"./data_raw/scopus_api/papers_df.csv")
  

  
  

# column name standardizing -----------------------------------------------

  
  # names(affils_df)
  source("name_standardizer.R")
  
  # ----- affils
  
  affils_df_1<-names_standardizer(affils_df_1) %>%
    ungroup() %>%
    select(-"@_fa") %>% 
    distinct(affil_id,affiliation,city,country,.keep_all = TRUE) %>% 
    mutate_all(tolower) %>% 
    mutate(country=as.factor(country),
           city=as.factor(city)) %>% 
    mutate(country=
             case_when(
               country == "united states" ~ "usa",
               .default = as.character(country)
             )
    ) %>% 
    mutate(source=gsub("./data_raw/scopus_api/affils/","",source)) %>% 
    mutate(source=gsub(" ","_",source)) %>% 
    remove_empty(c("rows", "cols"))
  
  write_rds(affils_df_1,"./data_intermediate/affils_df.rds")
  
  # ----- papers
  
  papers_df_1<-names_standardizer(papers_df_1) %>% 
    ungroup() %>%
    select(-"@_fa") %>% 
    # group_by(scopus_article_id,SO,TI) %>% 
    # tally() %>% 
    # arrange(desc(n)) %>% 
    distinct(scopus_article_id,SO,TI,.keep_all = TRUE) %>% 
    mutate_all(tolower) %>% 
    mutate(source=gsub("./data_raw/scopus_api/papers/","",source)) %>% 
    mutate(source=gsub(" ","_",source)) %>% 
    remove_empty(which = c("rows", "cols"))
  
  write_rds(papers_df_1,"./data_intermediate/papers_df.rds")
  
  # ----- authors 
  
  authors_df_1<-names_standardizer(authors_df_1) %>%
    ungroup() %>%
    select(-"@_fa",
           -"afid.@_fa") %>% 
    mutate(source=gsub("./data_raw/scopus_api/authors/","",source)) %>% 
    mutate(source=gsub(" ","_",source)) %>% 
    remove_empty(which = c("rows", "cols")) %>% 
    mutate_all(tolower)
  
  write_rds(authors_df_1,"./data_intermediate/authors_df.rds")
  
  
  
affils_df_1<-affils_df_1 %>% 
  select(-entry_no,
         -source,
         -affil_url)
  names(authors_df_1)
  

  # add refID ---------------------------------------------------------------

  # papers_df_1$refID <- seq.int(nrow(papers_df_1))
  # papers_df_1<-papers_df_1 %>% relocate(refID,.before=1)

  # 
  # # bring over the refiID from papers to authors ----------------------------
  # paper_ID_nos<-papers_df_1 %>% select(refID,source,entry_no)
  # 
  # 
  ###------------ 
  # If you run out of memory:
  # https://stackoverflow.com/questions/51248293/error-vector-memory-exhausted-limit-reached-r-3-5-0-macos
  ###------------
  # 
  # authors_df_1<-left_join(authors_df_1,paper_ID_nos,by="source") %>% 
  #   relocate(refID,.before=1)
  # affils_df_1<-left_join(affils_df_1,paper_ID_nos) %>% 
  #   relocate(refID,.before=1)
  # 
  # rm(paper_ID_nos)
  
  # cleanup -----------------------------------------------------------------
  
  # separate pubs with & without DOI ----------------------------------------
  
  pubs_no_doi<-papers_df_1 %>% 
    filter(is.na(DI)) %>% 
    distinct(source,SO,PY,TI,.keep_all=TRUE)
  
  pubs_no_doi_dupes<-pubs_no_doi %>% 
    group_by(source,SO,PY,TI) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  
  pubs_with_doi<-papers_df_1 %>% 
    filter(!is.na(DI)) %>% 
    distinct(source,DI,SO,PY,TI,.keep_all=TRUE)
  
  pubs_with_doi_dupes<-pubs_with_doi %>% 
    group_by(source,DI,SO,PY,TI) %>% 
    filter(!is.na(DI)) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    filter(n>1)
  # sum(pubs_with_doi_dupes$n)  
  
  
  papers_df_1<-bind_rows(pubs_no_doi,pubs_with_doi)
  
  
  rm(pubs_no_doi,
     pubs_with_doi,
     pubs_with_doi_dupes,
     pubs_no_doi_dupes)
  
  
  authors_df_1<-authors_df_1 %>% distinct(
    source,
    author_order,
    author_url,
    SID,
    AF,
    surname,
    given_name,
    first_middle_initials,
    affil_id,
    OI,
    .keep_all=TRUE)
  
  # affils_df<-affils_df[!duplicated(affils_df), ]
  
  
  
  # STANDARDIZE AUTHOR NAMES & ADDRESSES ------------------------------------
  source("author_name_cleaner.R")
  authors_affils<-author_name_cleaner(authors_affils)
  
  
  
  # ID PAPERS WITHOUT AUTHORS OR AUTHORS WITHOUT PAPERS ---------------------  
  
  # papers_df_1 %>% summarize(n=n_distinct(refID))
  # authors_df_1 %>% summarize(n=n_distinct(refID))
  # setdiff((papers_df_1$refID),(authors_df_1$refID))
  # setdiff((authors_df_1$refID),(papers_df_1$refID))
  # 
  
  # 
  # papers_df_1$refID<-as.character(papers_df_1$refID)
  # authors_df_1$refID
  # authors_df_1$refID<-as.character(authors_df_1$refID)
  # from_papers<-
  #   anti_join(papers_df_1 %>% select(refID),
  #             unique(authors_df_1 %>% select(refID)))
  # 
  # need_to_find_authors<- semi_join(papers_df_1,from_papers,by="refID")
  # 
  # from_authors<-
  #   anti_join(unique(authors_df_1 %>% select(refID)),
  #             papers_df_1 %>% select(refID))
  # 
  # need_to_find_papers<- semi_join(authors_df_1,from_authors,by="refID")
  # names(need_to_find_papers)
  # 
  # papers_df_1<-anti_join(papers_df_1,from_papers,by="refID")
  # authors_df_1<-anti_join(authors_df_1,from_authors,by="refID")
  # 
  # rm(from_papers,from_authors)
  # 
  # papers_df_1$PY<-papers_df_1$PY2
  # papers_df_1$PY<-as.numeric(papers_df_1$PY)
  # papers_df_1$PY2<-NULL
  # 
  # PY_binder<-papers_df_1 %>% select(refID,PY,PM)
  # 
  # authors_df_1<-left_join(authors_df_1,PY_binder,by="refID")
  # affils_df$refID<-as.character(affils_df$refID)
  # affils_df<-left_join(affils_df,PY_binder)
  # 
  # names(PY_binder)
  # names(affils_df)
  # affil_binder<-affils_df %>% select(affil_id)
  # affil_binder$affil_id<-as.character(affil_binder$affil_id)
  # authors_df_1<-left_join(authors_df_1,affil_binder)
  # 
  # write_rds(need_to_find_authors,"./data_intermediate/need_to_find_authors.rds")
  # write_rds(need_to_find_papers,"./data_intermediate/need_to_find_papers.rds")
  
  # SAVE CLEAN FILES --------------------------------------------------------
  
  
  
  authors_df_1 %>% summarize(n=n_distinct(refID))
  
  # ADD the Affiliation and Geo info to the original data set 
  # join by DOI
  # if necessary geocode what remains.

    
  
  
write_rds(papers_df_1,"./data_clean/papers_df_clean.rds")
  write_rds(authors_df_1,"./data_clean/authors_df_clean.rds")
  write_rds(affils_df_1,"./data_clean/affils_df_clean.rds")
    
  
# add geo of missing authors
  
all_georef <-read_rds("./data_clean/all_georef_clean.rds") %>% 
  arrange(refID,author_order)


all_georef_ok<-all_georef %>% 
  filter(address!="could not be extracted") 


all_georef_to_fix<-all_georef %>% 
  filter(address=="could not be extracted") %>% 
  select(-country,
         -address,
         -university,
         -department,
         -postal_code,
         -city,
         -state,
         -addr,
         -lat,
         -lon)


authors_affils<-left_join(authors_df_1,affils_df_1,by=c("affil_id")) %>% 
  select(-refID.y) %>% 
  rename(refID.x)

# ,"refID")) 
# 
# %>% 
#   mutate(refID.x=if_else(is.na(refID.x),refID.y,refID.x))


all_georef_to_fix<-left_join(all_georef_to_fix,authors_affils,by=c("refID","author_order"))
all_georef_to_fix<-all_georef_to_fix %>% 
  select(-OI.y) %>% 
  rename(OI=OI.x)

# all_georef_to_fix<-all_georef_to_fix %>% 
#   mutate(OI.x=if_else(is.na(OI.x),OI.y,OI.x)) %>% 
#   select(-OI.y) %>% 
#   rename(OI=OI.x)
  
all_georef_to_fix<-all_georef_to_fix %>% 
  unite("addr",
        city:country,
        sep=",",
        remove=FALSE)



missing_to_geocode<-all_georef_to_fix %>% 
  filter(is.na(latitude)) %>% 
  select(addr) %>% 
  distinct()

missing_to_geocode <- tidygeocoder::geocode(missing_to_geocode, addr, 
                                            method = "osm", lat = latitude, long = longitude)


all_georef_to_fix<-all_georef_to_fix %>% 
  left_join(missing_to_geocode,by="addr")



all_georef_to_fix<-all_georef_to_fix %>% 
  rename(lat="latitude.x",
         lon="longitude.x") %>% 
  mutate(lat=if_else(is.na(lat),latitude.y,lat)) %>% 
  mutate(lon=if_else(is.na(lon),longitude.y,lon)) %>% 
  select(-latitude.y,
         -longitude.y)




missing_to_geocode_country<-all_georef_to_fix %>% 
  filter(is.na(lat)) %>% 
  select(country) %>% 
  distinct()

missing_to_geocode_country<-
  tidygeocoder::geocode(missing_to_geocode_country, country, 
                        method = "osm", lat = latitude, long = longitude)




all_georef_to_fix<-left_join(all_georef_to_fix,
                               missing_to_geocode_country,
                               by="country")

all_georef_to_fix<-all_georef_to_fix %>% 
mutate(lat=if_else(is.na(lat),latitude,lat)) %>% 
  mutate(lon=if_else(is.na(lon),longitude,lon)) %>% 
  select(-latitude,
         -longitude)



all_georef_to_fix<-all_georef_to_fix %>% 
  mutate(lat=if_else(addr=="NA,NA",NA,lat)) %>% 
  mutate(lon=if_else(addr=="NA,NA",NA,lon)) %>% 
  mutate(addr=if_else(addr=="NA,NA",NA,addr)) 
           

names(all_georef_ok)
names(all_georef_to_fix)

all_georef_to_fix<-all_georef_to_fix %>% 
  mutate(lat=as.character(lat)) %>% 
  mutate(lon=as.character(lon))

# 
# all_georef_to_fix<-left_join(all_georef_to_fix,authors_affils,by=c("refID","author_order"))
# names(all_georef_to_fix)

all_georef_clean2<-bind_rows(all_georef_ok,all_georef_to_fix)


write_rds(all_georef_clean2,"./data_clean/all_georef_clean_2.rds")









  
  search_term_all<-all_pubs %>% 
    filter(refID %in% missing_add$refID) %>% 
    select(refID,DI,SO)
  
  search_term_all
  
  all_georef_missing <-all_georef_missing$missing_addresses 
  all_georef_missing <-all_georef_missing %>% 
    rename(city1=city,
           state1=state,
           AF=author_name) %>%
    separate_wider_delim(AF, 
                         ", ",
                         too_many = "merge",
                         too_few = "align_end",
                         names = c("surname", "given_name"),
                                   cols_remove = FALSE) %>% 
    mutate_all(tolower) %>% 
    separate_wider_delim(given_name, 
                         " ",
                         too_many = "merge",
                         too_few = "align_start",
                         names = c("given_name", "middle_name"),
                         cols_remove = FALSE) %>% 
    select(-country) 
  
  all_georef_missing
  authors_affils
  
  
  foo<-full_join(all_georef_missing, authors_affils,by=c("surname","author_order","given_name", "AF")) %>% 
  # foo<-full_join(all_georef_missing, authors_affils,by=c("surname","AF", "OI","given_name")) %>% 
    distinct() %>% 
    remove_empty(c("rows","cols"))
  
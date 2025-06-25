names_standardizer <- function(combined_data) {
  names(combined_data)[names(combined_data) == "prism:url"] <- "URL"
  names(combined_data)[names(combined_data) == "dc:identifier"] <- "scopus_article_id"
  names(combined_data)[names(combined_data) == "eid"] <- "eid"
  names(combined_data)[names(combined_data) == "dc:title"] <- "TI"
  names(combined_data)[names(combined_data) == "prism:publicationName"] <- "SO"
  names(combined_data)[names(combined_data) == "prism:issn"] <- "SN"
  names(combined_data)[names(combined_data) == "prism:volume"] <- "VL"
  names(combined_data)[names(combined_data) == "prism:issueIdentifier"] <- "IS"
  names(combined_data)[names(combined_data) == "prism:pageRange"] <- "page_range"
  names(combined_data)[names(combined_data) == "prism:coverDisplayDate"] <- "PY"
  names(combined_data)[names(combined_data) == "prism:doi"] <- "DI"
  names(combined_data)[names(combined_data) == "dc:description"] <- "AB"
  names(combined_data)[names(combined_data) == "citedby-count"] <- "TC"
  names(combined_data)[names(combined_data) == "pubmed-id"] <- "PI"
  names(combined_data)[names(combined_data) == "prism:aggregationType"] <- "PT"
  names(combined_data)[names(combined_data) == "subtypeDescription"] <- "DT"
  names(combined_data)[names(combined_data) == "authkeywords"] <- "DE"
  names(combined_data)[names(combined_data) == "source-id"] <- "scopus_source_id"
  names(combined_data)[names(combined_data) == "fund-no"] <- "funder_id"
  names(combined_data)[names(combined_data) == "openaccess"] <- "OA"
  names(combined_data)[names(combined_data) == "entry_number"] <- "entry_no"
  names(combined_data)[names(combined_data) == "article-number"] <- "article_number"
  names(combined_data)[names(combined_data) == "prism:eIssn"] <- "EI"
  names(combined_data)[names(combined_data) == "fund-sponsor"] <- "FU"
  names(combined_data)[names(combined_data) == "pii"] <- "UT" #elsevier_pub_identifier
  
  # from data_authors
  names(combined_data)[names(combined_data) == "@seq"] <- "author_order"
  names(combined_data)[names(combined_data) == "authid"] <- "SID"
  names(combined_data)[names(combined_data) == "authname"] <- "AF"
  names(combined_data)[names(combined_data) == "surname"] <- "surname"
  names(combined_data)[names(combined_data) == "given-name"] <- "given_name"
  names(combined_data)[names(combined_data) == "initials"] <- "first_middle_initials"
  names(combined_data)[names(combined_data) == "afid.$"] <- "affil_id"
  names(combined_data)[names(combined_data) == "orcid"] <- "OI"
  
  # from affiliations
  names(combined_data)[names(combined_data) == "afid"] <- "affil_id"
  names(combined_data)[names(combined_data) == "affilname"] <- "affiliation"
  names(combined_data)[names(combined_data) == "affiliation-city"] <- "city"
  names(combined_data)[names(combined_data) == "affiliation-country"] <- "country"
  
  
  
  
  # to be deleted
  names(combined_data)[names(combined_data) == "dc:creator"] <- "creator"
  names(combined_data)[names(combined_data) == "prism:coverDate"] <- "cover_date"
  names(combined_data)[names(combined_data) == "subtype"] <- "document_type_abbrev"
  names(combined_data)[names(combined_data) == "author-count.@limit"] <- "author_count_limit"
  names(combined_data)[names(combined_data) == "author-count.@total"] <- "author_count_total"
  names(combined_data)[names(combined_data) == "author-count.$"] <- "author_count"
  names(combined_data)[names(combined_data) == "openaccessFlag"] <- "open_access_tf"
  
  names(combined_data)[names(combined_data) == "freetoread.value.$"] <- "free_to_read"
  names(combined_data)[names(combined_data) == "freetoreadLabel.value.$"] <- "free_to_read_label"
  
  
  names(combined_data)[names(combined_data) == "fund-acr"] <- "funder_acronym"
  names(combined_data)[names(combined_data) == "author-url"] <- "author_url"
  names(combined_data)[names(combined_data) == "affiliation-url"] <- "affil_url"
  
  combined_data<-replace(combined_data, combined_data =="", NA)  
  # 
  combined_data<-combined_data %>%
    mutate(source=str_replace(source, "./data_raw/scopus_api/papers/scopus_", "")) %>%
    mutate(source=str_replace(source, "_papers", "")) %>%
    mutate(source=str_replace(source, "./data_raw/scopus_api/authors/scopus_", "")) %>%
    mutate(source=str_replace(source, "_authors", "")) %>%
    mutate(source=str_replace(source, "./data_raw/scopus_api/affils/scopus_", "")) %>%
    mutate(source=str_replace(source, "_affils", "")) %>%
    
    relocate(entry_no,.before = 1)
  
  return(combined_data)
}  
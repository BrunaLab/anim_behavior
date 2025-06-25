
author_name_cleaner <- function(authors_df) {
  
  ## AU -----
  authors_df$AU<-paste(authors_df$surname, authors_df$first_middle_initials, sep=", ")
  authors_df$AU<-gsub("[.]", "",authors_df$AU)
  
  ## AF -----
  authors_df$AF<-paste(authors_df$surname, authors_df$given_name, sep=", ")
  
  
  # ADRESSES ------
  
  # ADDRESS COLUMN to georef with refsplitr ---------------------------------
  
  
  # names(affils_df)
  authors_df$authorID<-authors_df$SID
  authors_df$state<-NA
  authors_df$postal_code<-NA
  
  return(authors_df)
}


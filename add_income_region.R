add_income_region <- function(DATASET) {
  
  library(countrycode)
  
  # IMPORT World Bank Data on country Income and Region
  # data downloaded 20260618 from https://datacatalog.worldbank.org/search/dataset/0037712
  wdi_data<-read_csv("./data_raw/WDI_CSV_2025_06_05/WDICountry.csv") %>% 
    mutate_all(tolower) %>% 
    clean_names(case = c("snake")) %>% 
    select(country_code,
           short_name,
           table_name,
           long_name,
           income_group,
           region)
  
  DATASET<-ab_georef$addresses
  DATASET<-DATASET %>% rename(country.name=country) %>% 
    mutate(country.name=
             case_when(
               country.name == "england" ~ "uk",
               country.name == "wales" ~ "uk",
               country.name == "northern ireland" ~ "uk",
               country.name == "scotland" ~ "uk",
               country.name == "wales" ~ "uk",
               country.name == "mi" ~ "usa",
               country.name == "mi" ~ "usa",
               country.name == "ny" ~ "usa",
               country.name == "pa" ~ "usa",
               country.name == "or" ~ "usa",
               country.name == "wi" ~ "usa",
      .default = as.character(country.name)
    )
    )
  
    
  
  
  DATASET<-DATASET %>% 
    mutate(country_code=countrycode(DATASET$country.name, origin = 'country.name', destination = 'iso3c')) %>% 
    relocate(c(country.name,country_code),.before=1) %>% 
    mutate_all(tolower) %>% 
    left_join(DATASET,wdi_data, by="country_code")
  
  # 
  # 
  # row.names(wdi_data) <- wdi_data$iso3c     #Assigning row names in table for later search
  # 
  #These lines add the income level and region level based on the editor country
  # DATASET<- DATASET %>% mutate(income_level=as.character() <- wdi_data[as.character(DATASET$geo.code), 'income']  #Making a new column of income level by country
  # DATASET$REGION <- wdi_data[as.character(DATASET$geo.code), 'region']  #Making a new column of income level by country
  # 
  #step 4: Changing the order of CATEGORY, INCOME_LEVEL, REGION and JOURNAL factors.
  #This is then used to have always the same order of the lines in future plots and tables
  incomes_ordered_list <- c(  'High income: OECD', 'High income: nonOECD',
                              'Upper middle income','Lower middle income','Low income')
  incomes_ordered_list<-tolower(incomes_ordered_list)
  #list of geographical regions, useful for analysis and to give them an order in plots
  regions_ordered_list <- c('North America', 'Europe & Central Asia','East Asia & Pacific',
                            'Latin America & Caribbean', 'Sub-Saharan Africa',
                            'South Asia','Middle East & North Africa')
  regions_ordered_list<-tolower(regions_ordered_list)
  DATASET$INCOME_LEVEL <-  factor(x =  DATASET$INCOME_LEVEL, levels = INCOMES.ORDERED.LIST)
  DATASET$REGION <-  factor(x =  DATASET$REGION, levels = REGIONS.ORDERED.LIST)
  
  # Northern Ireland is incorrectly coded as IRL instead of GBR
  DATASET$geo.code[DATASET$COUNTRY == "NORTH IRELAND"]  <- "GBR"    
  DATASET$geo.code[DATASET$COUNTRY == "NORTHERN IRELAND"]  <- "GBR" 
  DATASET$geo.code[DATASET$COUNTRY == "NORTH IRELAND"]  <- "GBR"
  DATASET$geo.code[DATASET$COUNTRY == "N. Ireland"]  <- "GBR" 
  DATASET$geo.code[DATASET$COUNTRY == "Northern Ireland"]  <- "GBR" 
  
  # rm(wdi_data,REGIONS.ORDERED.LIST,INCOMES.ORDERED.LIST)
  
  
  return(DATASET)
  
}
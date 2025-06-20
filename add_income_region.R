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
  
  # DATASET<-all_georef
  DATASET<-DATASET %>% rename(country.name=country) %>% 
    mutate(country.name=
             case_when(
               country.name == "england" ~ "uk",
               country.name == "wales" ~ "uk",
               country.name == "northern ireland" ~ "uk",
               country.name == "scotland" ~ "uk",
               country.name == "mi" ~ "usa",
               country.name == "mi" ~ "usa",
               country.name == "ny" ~ "usa",
               country.name == "pa" ~ "usa",
               country.name == "or" ~ "usa",
               country.name == "wi" ~ "usa",
               country.name == "il" ~ "usa",
               country.name == "cent afr republ" ~ "central african republic",
               # country.name == "yugoslavia" ~ "---",
               # country.name == "w ind assoc st" ~ "west indies",
               country.name == "yugoslavia" ~ NA,
               country.name == "w ind assoc st" ~ NA,
               
      .default = as.character(country.name)
    )
    )
  
    
  
  
  DATASET<-DATASET %>% 
    mutate(country_code=countrycode(DATASET$country.name, origin = 'country.name', destination = 'iso3c')) %>% 
    relocate(c(country.name,country_code),.before=1) %>% 
    mutate_all(tolower) %>% 
    left_join(wdi_data, by="country_code") %>% 
    relocate(region,.after="country_code") %>% 
    relocate(income_group,.after="region")
  
  #step 4: Changing the order of CATEGORY, INCOME_LEVEL, REGION and JOURNAL factors.
  #This is then used to have always the same order of the lines in future plots and tables
  incomes_ordered_list <- c(  'high income','upper middle income','lower middle income','low income')
  
  #list of geographical regions, useful for analysis and to give them an order in plots
  regions_ordered_list <- c('north america', 'europe & central asia','east asia & pacific',
                            'latin america & caribbean', 'sub-saharan africa',
                            'south asia','middle east & north africa')
  
  DATASET$income_group <-  factor(x =  DATASET$income_group, levels = incomes_ordered_list)
  DATASET$region <-  factor(x =  DATASET$region, levels = regions_ordered_list)
  
  
  
  # Northern Ireland is incorrectly coded as IRL instead of GBR
  # DATASET$geo.code[DATASET$COUNTRY == "NORTH IRELAND"]  <- "GBR"    
  # DATASET$geo.code[DATASET$COUNTRY == "NORTHERN IRELAND"]  <- "GBR" 
  # DATASET$geo.code[DATASET$COUNTRY == "NORTH IRELAND"]  <- "GBR"
  # DATASET$geo.code[DATASET$COUNTRY == "N. Ireland"]  <- "GBR" 
  # DATASET$geo.code[DATASET$COUNTRY == "Northern Ireland"]  <- "GBR" 
  # 
  # rm(wdi_data,REGIONS.ORDERED.LIST,INCOMES.ORDERED.LIST)
  
  
  return(DATASET)
  
}
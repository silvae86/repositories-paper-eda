if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(readxl,data.table,dplyr,lsa,text2vec,stringr,readODS)

threshold <- 0.40

Comparative <- read_excel("Comparative.xlsx", 
                          sheet = "Cross", col_names = FALSE)

rda_parameters <- transpose(Comparative[1:4,])
rda_parameters <- na.omit(rda_parameters)
colnames(rda_parameters) <- c("category", "feature", "description", "weight")
rownames(rda_parameters)<- c(1:nrow(rda_parameters))

dataverse_parameters <- Comparative[1:2]
dataverse_parameters <- na.omit(dataverse_parameters)
colnames(dataverse_parameters) <- c("category", "description")
dataverse_parameters <- dataverse_parameters %>% 
  filter(category != 'Business model' & description != "Road Maps" & description != "Preservation technology?")

all_parameters <- append(rda_parameters$"description", dataverse_parameters$"description")

select_and_add_platforms_column <- function(df, column_name)
{
  df <- df %>% select(c(
    'Analyze Boston (CKAN)',	
    'data.world',	
    'Dryad',	
    'figshare',	
    'Harvard Dataverse',	
    'Mendeley Data',	
    'Open ICPSR',
    'Zenodo',	
    'Open Science Framework',
    'Dendro'
  )) 
  
  df <- t(df)
  df <- cbind(rownames(df), df[,1])
  rownames(df) <- c(1:nrow(df))
  colnames(df) <- c("platform", column_name)
  df
}

open_source <- read_excel("Comparative.xlsx", 
                          sheet = "Comparative review of data repo", col_names = T) %>%
                          filter(`Software Features` == "Open Source")
  
open_source <- select_and_add_platforms_column(open_source, "open_source")                          

free <- read_excel("Comparative.xlsx", 
                          sheet = "Comparative review of data repo", col_names = T) %>%
                          filter(`Software Features` == 'Free To Use')

free <- select_and_add_platforms_column(free, "free")  


n_datasets <- read_excel("Comparative.xlsx", 
                   sheet = "Comparative review of data repo", col_names = T) %>%
  filter(`Software Features` == 'Total # of published datasets as of July 2017')

n_datasets <- select_and_add_platforms_column(n_datasets, "n_datasets")


n_files <- read_excel("Comparative.xlsx", 
                       sheet = "Comparative review of data repo", col_names = T) %>%
  filter(`Software Features` == 'Total # of published files as of May 2017')

n_files <- select_and_add_platforms_column(n_files, "n_files")


n_users <- read_excel("Comparative.xlsx", 
                      sheet = "Comparative review of data repo", 
                      col_names = T) %>%
  filter(`Software Features` == 'Total # of public users as of May 2017')

n_users <- select_and_add_platforms_column(n_users, "n_users")

certified <- read_excel("Comparative.xlsx", 
                      sheet = "Comparative review of data repo", 
                      col_names = T) %>%
                      filter(`Software Features` == 'Certification?')

certified <- select_and_add_platforms_column(certified, "certified")

infrastructure <- read_excel("Comparative.xlsx", 
                        sheet = "Comparative review of data repo", 
                        col_names = T) %>%
                        filter(`Software Features` == 'Infrastructure is turnkey, installable on premises, or both?')

infrastructure <- select_and_add_platforms_column(infrastructure, "infrastructure")

infrastructure[infrastructure %like% "Both"] <- "SaaS + Local"
infrastructure[infrastructure %like% "Turnkey"] <- "SaaS"
infrastructure[infrastructure %like% "Premises"] <- "Local"


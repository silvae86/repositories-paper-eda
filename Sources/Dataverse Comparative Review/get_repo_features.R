### Get Repository characteristics

repoFeatures <- read_excel("Comparative.xlsx", 
                           sheet = "Comparative review of data repo", 
                           col_names = TRUE)

repoFeatures["Categories"] <- NULL
repoFeatures <- as.matrix(repoFeatures)
featureNames <- repoFeatures[,1]
repoFeatures[repoFeatures %like% "Yes" | 
               repoFeatures %like% "Sort of" | 
               repoFeatures %like% "All file types"| 
               repoFeatures %like% "All data types and formats within reason"] <- 1


repoFeatures[repoFeatures %like% "No" | 
               repoFeatures %like% "NA" | 
               repoFeatures %like% "Unknown" | 
               repoFeatures %like% "Planned"] <- 0

repoFeatures[,1] <- featureNames
repoFeatures <- na.omit(repoFeatures)
repoFeatures <- as.data.table(repoFeatures) %>%
  select(c('Software Features',
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

columns <- colnames(repoFeatures)
columns[1] <- "dataverse_feature"
colnames(repoFeatures) <- columns

#write.table(repoFeatures, file = "repofeatures.xls", sep = "#", na = "0", row.names = F, col.names = T)


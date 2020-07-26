source("./get_repo_features.R")

# read manually validated table
sim <- readODS::read_ods(path = "similarities_lsa_validated.ods", sheet = "Sheet1", col_names = T)

matches <- melt(data = data.table(sim), measure.vars = dataverse_parameters$description, na.rm = T)
matches$feature <- NULL
colnames(matches) <- c("rda_feature", "weight", "dataverse_feature", "match")
               
merged <- merge(x = matches, y=repoFeatures, by="dataverse_feature")

merged[,3:ncol(merged)] <- merged %>%
  select(c('weight',
           'match',
           'Analyze Boston (CKAN)',	
           'data.world',	
           'Dryad',	
           'figshare',	
           'Harvard Dataverse',	
           'Mendeley Data',	
           'Open ICPSR',
           'Zenodo',	
           'Open Science Framework'
  )) %>%
  mutate_all(as.numeric)

merged[,5:ncol(merged)] <- merged[,5:ncol(merged)] * merged$weight

totals <- colSums(merged[,5:ncol(merged)])


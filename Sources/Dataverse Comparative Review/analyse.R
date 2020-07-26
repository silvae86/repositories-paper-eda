if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# For Ubuntu 20.04 focal:
# sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
# sudo apt update
# sudo apt install r-base r-base-core r-recommended r-base-dev
# sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
# sudo apt update
# sudo apt install r-cran-rgl r-cran-rjags r-cran-snow r-cran-ggplot2 r-cran-igraph r-cran-lme4 r-cran-rjava r-cran-devtools r-cran-roxygen2 r-cran-rjava
pacman::p_load(readxl,data.table,dplyr,lsa,text2vec,stringr,ggplot2,readODS)

source("./read_data.R")

# read manually validated table
sim <- readODS::read_ods(path = "similarities_validated.ods", sheet = "Sheet1", col_names = T, row_names = F)

matches <- melt(data = data.table(sim), 
                measure.vars = dataverse_parameters$description,
                na.rm = T)

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
           'Open Science Framework',
           'Dendro'
  )) %>%
  mutate_all(as.numeric)

merged[,5:ncol(merged)] <- merged[,5:ncol(merged)] * merged$weight

totals <- colSums(merged[,5:ncol(merged)])
totals <- as.data.frame(totals)
totals$platform <- rownames(totals)
rownames(totals) <- c(1:nrow(totals))
plot <- ggplot(data=totals)
View(plot)

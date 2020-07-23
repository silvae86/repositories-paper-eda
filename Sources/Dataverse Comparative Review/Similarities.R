if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

prep_fun = function(x) {
  # make text lower case
  x = str_to_lower(x)
  # remove non-alphanumeric symbols
  x = str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  str_replace_all(x, "\\s+", " ")
  str_replace_all(x, "allow", " ")
  str_replace_all(x, "ability", " ")
  str_replace_all(x, "support", " ")
}

pacman::p_load(readxl,data.table,stringr,futile.options,tm,stats,lsa,dplyr)

threshold <- 0.10

Comparative <- read_excel("Comparative.xlsx", 
                          sheet = "Cross", col_names = FALSE)

rda_parameters <- transpose(Comparative[1:4,])
rda_parameters <- na.omit(rda_parameters)
colnames(rda_parameters) <- c("category", "feature", "description", "weight")
rda_parameters$"description" <- paste(rda_parameters$"feature", rda_parameters$"description")
rownames(rda_parameters)<- c(1:nrow(rda_parameters))

dataverse_parameters <- Comparative[1:2]
dataverse_parameters <- na.omit(dataverse_parameters)
colnames(dataverse_parameters) <- c("category", "description")
dataverse_parameters$"description" <- paste(dataverse_parameters$"category", dataverse_parameters$"description")

all_parameters <- append(rda_parameters$"description", dataverse_parameters$"description")

all_parameters_corpus <- SimpleCorpus(
  VectorSource(
    prep_fun(all_parameters)
  ), control = list(language='en')
)

all_parameters_Tfidf <- DocumentTermMatrix(
  all_parameters_corpus, 
  control = list(weighting = weightTfIdf, 
                 stopwords (kind = "en"), 
                 stemming = TRUE, 
                 removePunctuation = T,
                 removeNumbers = T
            )
)
all_parameters_Matrix <- as.matrix(all_parameters_Tfidf)
rownames(all_parameters_Matrix) <- all_parameters
sim <- t(cosine(t(all_parameters_Matrix)))

nCol <- ncol(sim)
sim <- sim[c(1:nrow(rda_parameters)),c(nrow(rda_parameters):nCol)]
sim <- sim[,-1]

sim <- cbind(rda_parameters$"feature", rda_parameters$"weight", sim)
colnames(sim) <- append(c("rda_feature", "weight"), dataverse_parameters$"description")

# filter out all those records under the threshold
sim[sim < threshold] <- NA


  
rm(all_parameters, all_parameters_corpus, all_parameters_Tfidf, prep_fun, nCol, Comparative, threshold, rda_parameters, dataverse_parameters, all_parameters_Matrix)
### Get Repository characteristics

repoFeatures <- read_excel("Comparative.xlsx", 
                          sheet = "Comparative review of data repo", 
                          col_names = TRUE)

repoFeatures["Categories"] <- NULL
repoFeatures <- as.matrix(repoFeatures)
featureNames <- repoFeatures[,1]
repoFeatures[!(repoFeatures %ilike% "%Yes%") ] <- 0
repoFeatures[repoFeatures %ilike% "%Yes%"] <- 1
repoFeatures[,1] <- featureNames
repoFeatures <- na.omit(repoFeatures)
repoFeatures <- repoFeatures[,c(1:(ncol(repoFeatures)-5))]

melted <- melt(data.table(sim), na.rm = T, id.vars="rda_feature")
colnames(melted) <- c("rda_feature", "dataverse_feature", "similarity")

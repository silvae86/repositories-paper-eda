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
}

pacman::p_load(readxl,data.table,stringr,futile.options,tm,stats,lsa)

threshold <- 0.2

Comparative <- read_excel("Comparative.xlsx", 
                          sheet = "Cross", col_names = FALSE)

rda_parameters <- transpose(Comparative[1:2,])
rda_parameters <- na.omit(rda_parameters)
rownames(rda_parameters)<- c(1:nrow(rda_parameters))
colnames(rda_parameters) <- c("1", "2")
dataverse_parameters <- Comparative[1:2]
dataverse_parameters <- na.omit(dataverse_parameters)
colnames(dataverse_parameters) <- c("1", "2")

rm(Comparative)

all_parameters <- append(rda_parameters$"2", dataverse_parameters$"2")

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
sim <- t(cosine(t(all_parameters_Matrix)))

nCol <- ncol(sim)
sim <- sim[c(1:nrow(rda_parameters)),c(nrow(rda_parameters):nCol)]
sim <- sim[,-1]
rm(nCol)

rownames(sim) <- rda_parameters$"2"
colnames(sim) <- dataverse_parameters$"2"

# filter out all those records under the threshold
sim[sim < threshold] <- ""

write.table(x = sim, file = "similarities.xls", na = "")



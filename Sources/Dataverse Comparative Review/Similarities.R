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
colnames(rda_parameters) <- c("1", "2", "3")
dataverse_parameters <- Comparative[1:2]
dataverse_parameters <- na.omit(dataverse_parameters)
colnames(dataverse_parameters) <- c("1", "2")

all_parameters <- rbind(rda_parameters$'2', dataverse_parameters$'2')
all_parameters_corpus <- SimpleCorpus(
  VectorSource(
    prep_fun(all_parameters)
  ), control = list(language='en')
)

all_parameters_Tfidf = DocumentTermMatrix(all_parameters_corpus, control = list(weighting = weightTfIdf))
all_parameters_Matrix = t(as.matrix(all_parameters_Tfidf))

sim <- cosine(all_parameters_Matrix)
sim <- as.matrix(dist)
sim[sim <= threshold] <- NA
rownames(sim) <- colnames(sim) <- all_parameters



# subset <- sim[nrow(rda_parameters):ncol(dist), 1:nrow(rda_parameters)]

write.table(x = subset, file = "similarities.xls", na = "")



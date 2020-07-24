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

pacman::p_load(readxl,data.table,stringr,futile.options,tm,stats,text2vec,dplyr)

threshold <- 0.40

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

rda_parameters.tokens = itoken(rda_parameters$"description", progressbar = FALSE)
dataverse_parameters.tokens = itoken(dataverse_parameters$"description", progressbar = FALSE)

all_features <- append(rda_parameters$"feature", dataverse_parameters$"description")
all_parameters <- append(rda_parameters$"description", dataverse_parameters$"description")
all_parameters <- prep_fun(all_parameters)
  
all_parameters.tokens <- itoken(all_parameters, progressbar = FALSE)
v = create_vocabulary(all_parameters.tokens, stopwords = stopwords_en)
v = prune_vocabulary(v, doc_proportion_max = 0.6, term_count_min = 4)
vectorizer = vocab_vectorizer(v)

dtm = create_dtm(all_parameters.tokens, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)

lsa = LSA$new(n_topics = 20)
dtm_tfidf_lsa = fit_transform(dtm_tfidf, lsa)

sim = sim2(x = dtm_tfidf_lsa, method = "cosine", norm = "l2")

rownames(sim) <- colnames(sim)<- all_features

sim[sim < threshold] <- NA
sim <- sim[1:nrow(rda_parameters),(nrow(rda_parameters)+1):ncol(sim)]

write.table(sim, file = "similarities_lsa.xls", sep = "#", na = "")

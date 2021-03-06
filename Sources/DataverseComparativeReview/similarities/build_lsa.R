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

# For Ubuntu 20.04 focal:
# sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
# sudo apt update
# sudo apt install r-base r-base-core r-recommended r-base-dev
# sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
# sudo apt update
# sudo apt install r-cran-rgl r-cran-rjags r-cran-snow r-cran-ggplot2 r-cran-igraph r-cran-lme4 r-cran-rjava r-cran-devtools r-cran-roxygen2 r-cran-rjava
pacman::p_load(readxl,data.table,dplyr,lsa,text2vec,stringr,readODS)

source("./repoFeatures.R")

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

lsa = LSA$new(n_topics = 10)
dtm_tfidf_lsa = fit_transform(dtm_tfidf, lsa)

sim = sim2(x = dtm_tfidf_lsa, method = "cosine", norm = "l2")


sim[sim < threshold] <- NA
sim <- sim[1:nrow(rda_parameters),(nrow(rda_parameters)+1):ncol(sim)]
sim <- cbind(rda_parameters, sim)
colnames(sim) <- append(colnames(rda_parameters), dataverse_parameters$description)
sim$category <- NULL

# Joao Rocha da Silva, Jul 2020
# replace with "similarities_lsa_validated.xlsx" if you want to destroy 
# the manually validated results and restart
# did this because LSA is an automated method; some matches are incorrect.
write_ods(sim, path = "similarities_lsa.ods", row_names = F)
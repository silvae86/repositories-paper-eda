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
pacman::p_load(readxl,data.table,dplyr,lsa,text2vec,stringr,ggplot2,readODS,grid,gridExtra,reshape2,emojifont,scales)

source("./read_data.R")
source("./get_repo_features.R")

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
totals <- merge(totals, open_source)
totals <- merge(totals, free)
totals <- merge(totals, n_datasets)
totals <- merge(totals, n_files)
totals <- merge(totals, n_users)
totals <- merge(totals, certified)
totals <- merge(totals, infrastructure)

colnames(totals) <- c("platform","score", "open_source", "free", "n_datasets", "n_files", "n_users", "certified", "infrastructure")

underGraphTableTotals <- t(totals %>%
                             arrange(desc(score)) %>%
                             select(c("open_source", "free", "n_datasets", "n_files", "n_users", "certified", "infrastructure")))

# text wrapping for columns of the table under the graph

colwidth <- 7

wrap_text <- function(names, colwidth)
{
  names <- sapply(names, function(x) paste(strwrap(x, width = colwidth),  collapse="\n"))
  names
}

rownames(underGraphTableTotals) <- wrap_text(
  c("Código Aberto", 
    "Livre (\"Free To Use\")",
    "Núm. Conjuntos Dados (6/2017)",
    "Num. Ficheiros (5/2017)",
    "Num. Utilizadores (5/2017)",
    "Certificação",
    "Infraestrutura"
  ), 10)
colnames(underGraphTableTotals) <- wrap_text((totals %>% arrange(desc(score)))$platform, colwidth)

underGraphTableTotals[underGraphTableTotals[,] == "No"] <- "Não"
underGraphTableTotals[underGraphTableTotals[,] == "Yes"] <- "Sim"
underGraphTableTotals[underGraphTableTotals[,] == "Unknown"] <- "N/D"
underGraphTableTotals[underGraphTableTotals[,] == "Yes - Data Seal of Approval; World Data System"] <- "Sim\n (DSA+WDS)"
underGraphTableTotals[underGraphTableTotals[,] == "No - Currently working toward ISO certification."] <- "Não - ISO \nno futuro"

# Create a table
thm <- ttheme_default(colhead = 
                        # first unit is the width, and second the height
                        list(fg_params=list(cex = 0.9), padding=unit.c(unit(12, "mm"), unit(3, "mm"))),
)

tab = tableGrob(underGraphTableTotals, 
                rows=rownames(underGraphTableTotals),
                cols = colnames(underGraphTableTotals),
                theme=thm,
                vp = NULL)

# create plot with table underneath
# margin: unit(c(top, right, bottom, left), units)
p <- ggplot(totals, aes(x=reorder(platform,-score), y=score))+
  geom_bar(stat='identity', fill="plum")+
  ylab("Pontuação de funcionalidades") + 
  xlab(NULL) + 
  theme(axis.text = element_text(size=11)
        #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  ) +
  # wrap labels
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  geom_text(data=totals, aes(x=reorder(platform,-score), y=(score - 1000), label = score), vjust=0)+
  theme(plot.margin=unit(c(1.7,1,14,2),"cm")) + 
  annotation_custom(tab, xmin=1, xmax=9, ymin=-10000, ymax=-23000)

ggsave("plot.pdf", plot = p, scale = 1, width = 13.1, height = 9.03, units = "in", dpi = 300, limitsize = T, device=NULL)
# ==============================================================================
# structural topic models with web of science data 
# ==============================================================================

# preliminaries ================================================================

# first load data in wos_descriptives.R

# load packages
library(stm)
library(here)

# set seed
set.seed(1234)

# load data into stm object ====================================================

# preprocess
ethics_processed <- textProcessor(documents = ethics_data$abstract,
                           metadata = ethics_data,
                           removenumbers = FALSE,
                           removepunctuation = FALSE,
                           ucp = FALSE)

ethics_out <- prepDocuments(ethics_processed$documents, 
                     ethics_processed$vocab, 
                     ethics_processed$meta)
ethics_docs <- ethics_out$documents
ethics_vocab <- ethics_out$vocab
ethics_meta <- ethics_out$meta

# check term frequency 
plotRemoved(ethics_processed$documents, lower.thresh = seq(1, 50, by = 1))
ethics_out <- prepDocuments(ethics_processed$documents, 
                            ethics_processed$vocab,
                            ethics_processed$meta, 
                            lower.thresh = 5)

# topic number diagnostics 
ethics_k_diag <- searchK(out$documents, 
                         out$vocab, 
                         K = seq(5, 20, 1), 
                         data = meta, 
                         cores = 5L)


# estimate stm on ethics data ==================================================

ethics_k10 <- stm(documents = ethics_out$documents, 
                  vocab = ethics_out$vocab,
                  K = 9, 
                  max.em.its = 100, 
                  data = ethics_out$meta,
                  init.type = "Spectral")

# visualize results : prevalence
plot(ethics_k10, labeltype = 'frex', n = 5)

# get two quotes per topic
thoughts <- findThoughts(ethics_k10, texts = ethics_out$meta$title, 
             topics = c(1:9), n = 3)

# plot quotes for each topic
plotQuote(thoughts$docs$`Topic 1`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 2`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 3`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 4`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 5`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 6`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 7`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 8`, maxwidth = 300, width = 50)
plotQuote(thoughts$docs$`Topic 9`, maxwidth = 300, width = 50)

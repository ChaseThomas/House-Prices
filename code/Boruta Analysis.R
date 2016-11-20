library(Boruta)

sample <- read.csv(file.path("../data/cleaned_train.csv"),stringsAsFactors = FALSE)

candidate.features <- setdiff(names(sample),c("Id","SalePrice"))

# pull out the response variable
response <- sample$SalePrice

# remove identifier and response variables
sample <- sample[candidate.features]

bor.results <- Boruta(sample,response, maxRuns=200, doTrace=0)

boruta.df <- attStats(bor.results)

write.csv(boruta.df, "../data/boruta_output.csv", row.names = TRUE)
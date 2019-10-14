lib = read.csv("./public_libraries.csv", stringsAsFactors = F)
head(lib)
#View(df)

#class(df$Longitude)

sample_data <- lib[c(1:1000),]
saveRDS(sample_data, "./sample_lib.rds")


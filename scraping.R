library(rvest)
library(tidyverse)

url <- "https://ifrafragrance.org/safe-use/library"

data <- read_html(url) %>%
    html_table()

table <- data[[1]]
names(table) <- c("1", "2", "cas_no", "title", "type", "publication", "amendment", "3")
table <- table %>%
    select(!c("1", "2", "3")) %>%
    mutate(publication = gsub("^$", NA, table$publication))

restrictions <- filter(table, type == "R")$title

jsons <- list()
for (name in restrictions) {
    jsons[[name]] <- makeJson(name)
}

#jsons <- list()
#for (i in 1:length(restrictions)) {
#    name <- restrictions[i]
#    jsons[[name]] <- makeJson(name)
#}
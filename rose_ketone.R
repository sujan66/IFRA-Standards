library(tabulizer)
library(tidyverse)
library(jsonlite)

compound_name <- "Rose ketones"
filePath <- paste("Standards/", compound_name, ".pdf", sep = "")
data <- extract_tables(filePath, pages = 1:4)
data2 <- extract_text(filePath, pages = 2:3)

table1 <- data[[1]]
table2 <- data[[4]]

strings <- unlist(str_split(data2, "\r\n"))
strings <- strings[-c(1:7, 54:61, 86:101)]
strings <- str_trim(strings)

casNo <- table1[1:(which(table1 == "Synonyms:") - 1), 2]
casNo <- subset(casNo, grepl("[0-9]+-[0-9]+-[0-9]+", casNo))
casNo <- gsub(" [A-Za-z:]+", "", casNo)
if (length(casNo) == 1)
    casNo <- unbox(casNo)

synonyms <- c(table1[(which(table1 == "Synonyms:")):nrow(table1) , 2], strings)
#synonyms <- unlist(strsplit(synonyms, ", "))
index <- grep("[0-9]+-[0-9]+-[0-9]+ \\([A-Z0-9]+\\):", synonyms)
if (length(index) > 0)
    synonyms <- synonyms[-(grep("[0-9]+-[0-9]+-[0-9]+ \\([A-Z0-9]+\\):", synonyms))]
#index <- grep("[-]$", synonyms)
#for (i in index) {
#    synonyms[i] <- paste(synonyms[i], synonyms[i + 1], sep = "")
#}
#if (length(index) > 0)
#    synonyms <- synonyms[-(index + 1)]
if (length(synonyms) == 1)
    synonyms <- unbox(synonyms)

molecularFormula <- table1[1:2, 3]

restriction_table <- table2[-c(1, 11:14), ]
restriction_table <- gsub(" %", "", restriction_table)
restriction_table <- str_split_fixed(restriction_table, "(?<=[0-9A-D]{1}) ", 4)
restriction_table <- rbind(restriction_table[, 1:2], restriction_table[, 3:4])
colnames(restriction_table) <- c("Categories", "Limits_in_percentage")
restriction_table <- as_tibble(restriction_table)

compound <- list(unbox(compoundName), casNo, synonyms,
                 molecularFormula, restriction_table)
names(compound) <- c("compound_name", "cas_no", "synonyms", "molecular_formula",
                     "restrictions")

jsons[compound_name] = toJSON(compound)
library(tabulizer)
library(dplyr)
library(stringr)
library(jsonlite)

makeJson <- function(compound_name) {
    
    out <- tryCatch({
    
            filePath <- paste("Standards/", compound_name, ".pdf", sep = "")
            data <- extract_tables(filePath, pages = 1:2)
            
            table1 <- data[[1]]
            if (is.null(nrow(data[[4]]))) {
                table2 <- NULL
            } else 
                table2 <- data[[4]][-1, ]
            if (length(data) > 4) {
                table3 <- data[[5]]
            } else
                table3 <- NULL
            
            compoundName <- compound_name
            
            casNo <- table1[1:(which(table1 == "Synonyms:") - 1), 2]
            casNo <- subset(casNo, grepl("[0-9]+-[0-9]+-[0-9]+", casNo))
            casNo <- gsub(" [A-Za-z:]+", "", casNo)
            if (length(casNo) == 1)
                casNo <- unbox(casNo)
            
            synonyms <- table1[(which(table1 == "Synonyms:")):nrow(table1) , 2]
            #synonyms <- unlist(strsplit(synonyms, ", "))
            index <- grep("[0-9]+-[0-9]+-[0-9]+:", synonyms)
            if (length(index) > 0)
                synonyms <- synonyms[-(grep("[0-9]+-[0-9]+-[0-9]+:", synonyms))]
            #for (i in length(synonyms):1) {
            #    temp <- grepl("[-]$", synonyms[i])
            #    if (temp) {
            #        synonyms <- synonyms[-i]
            #    } else
            #        break
            #}
            #index <- grep("[-]$", synonyms)
            #for (i in index) {
            #    synonyms[i] <- paste(synonyms[i], synonyms[i + 1], sep = "")
            #}
            #if (length(index) > 0)
            #    synonyms <- synonyms[-(index + 1)]
            if (length(synonyms) == 1)
                synonyms <- unbox(synonyms)
            
            if (ncol(table1) == 4)
                molecularFormula <- table1[1, 4]
            else 
                molecularFormula <- table1[1, 3]
            
            if (!is.null(table2)) {
                res_one <- gsub(" %", "", table2)
                if (!is.null(nrow(res_one)))
                    res_one <- paste(res_one[, 1], res_one[, 2])
                else
                    res_one <- paste(res_one[1], res_one[2])
                res_one <- str_split_fixed(res_one, "(?<=[0-9A-D]{1}) ", 4)
            } else
                res_one <- matrix()
            
            if (!is.null(table3)) {
                res_two <- gsub(" %", "", table3)
                if (is.matrix(res_two) && ncol(res_two) != 4) {
                    res_two <- subset(res_two, grepl("^Category", res_two[, 1]))
                    res_two <- rbind(str_split_fixed(res_two[,1], "(?<=[0-9A-D]{1}) ", 2), 
                                     res_two[, 2:3])
                }
            } else
                res_two <- NULL
            
            if (is.matrix(res_two) && ncol(res_two) == 4) {
                restriction_table <- rbind(res_one, res_two)
                restriction_table <- rbind(restriction_table[, 1:2], 
                                           restriction_table[, 3:4])
            } else {
                res_one <- rbind(res_one[, 1:2], res_one[, 3:4])
                restriction_table <- rbind(res_one, res_two)
            }
            colnames(restriction_table) <- c("Categories", "Limits_in_percentage")
            restriction_table <- as_tibble(restriction_table)
            
            compound <- list(unbox(compoundName), casNo, synonyms,
                             unbox(molecularFormula), restriction_table)
            names(compound) <- c("compound_name", "cas_no", "synonyms", 
                                 "molecular_formula", "restrictions")
            
            json <- toJSON(compound)
            json
        },
        error = function(cond) {
            message(paste("error in compound:", compound_name))
            message(cond)
            return(NA)
        },
        warning = function(cond) {
            message(paste("warning in compound:", compound_name))
            message(cond)
            return(NULL)
        },
        finally = message(paste("\ncompound", compound_name, "done"))
    )
    
    return(out)
}
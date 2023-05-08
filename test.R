rm(list = ls())

library(tidyverse)
library(writexl)

# excelhez
a <- str_split_fixed(c("a|b|c", "a|b", "a"), "\\|", 2)


# dataframehez
l <- str_split(c("a|b|c", "a|b", "a"), "\\|")
df <- data.frame(c(1, 2, 3))
df$ajker <- l


write_xlsx(df, path = "ex.xlsx")

save(df, file = "df.Rdata")
load("df.Rdata")




# egységesítés - próba
# Define your list of names
names <- list(c("John", "Adam"), c("Mike"), c("Addam"), c("Adam", "Eve"), c("Eve"))

# Define your list of IDs
ids <- list(c("id1", "id2"), c("id3"), c("id2"), c("", ""), c(""))

testdf <- data.frame(1:5)
testdf$n <- names
testdf$i <- ids


unique_names <- list()
solution <- testdf

for (row_n in 1:length(testdf$i)) {
  for (id_n in 1:length(testdf$i[[row_n]])) {
    id <- testdf$i[[row_n]][[id_n]]
    if (id == "") next
    name <- testdf$n[[row_n]][[id_n]]
    if (!is.null(unique_names[[id]])) {
      solution$n[[row_n]][[id_n]] <- unique_names[[id]]
      print(paste("changed", name, "to", unique_names[[id]]))
      next
    }
    unique_names[id] <- name
  }
}

print(unique_names)
print(solution)


adoszam_ex <- c("13588098241,HU13588098",
                "CZ27865410",
                "12070907243,HU12070907,01-09-462762|10240616242,HU 10240616|11961583241,HU11961583")

adoszam_df <- data.frame(adoszam_ex)
adoszam_df$eu <- NA
adoszam_df$cegj <- NA
adoszam_df$hun <- list(NA)
adoszam_df$split <- str_split(adoszam_df$adoszam_ex, pattern = "\\||,")

for (row_n in 1:length(adoszam_df$split)) {
  row <- adoszam_df$split[[row_n]]
  for (id in row) {
    if (str_starts(id, pattern = "[:upper:]{2}")) {
      if (is.na(adoszam_df$eu[row_n])) {
        adoszam_df$eu[row_n] <- id
      } else {
        
      adoszam_df$eu[row_n] <- c(adoszam_df$eu[row_n], id)
      }
    }
    if (str_starts(id, pattern = "\\d{2}-")) {
      adoszam_df$cegj[row_n] <- id
    }
    if (str_starts(id, pattern = "(\\d{8}-)|\\d{11}")) {
      adoszam_df$hun[row_n] <- id
    }
  }
}

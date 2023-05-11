rm(list = ls())

library(tidyverse)
library(writexl)

# load data
data <- read.csv("data/Export.csv")
tib <- as_tibble(data)

# drop unneccessary columns

# transforming function
split_by_divider <- function(vect, num_of_columns = 1, column_prefix = "") {
  if (num_of_columns != 1) {
    m <- str_split_fixed(vect, pattern = "\\|", n = num_of_columns)
    # name columns based on prefix
    column_names <- paste(column_prefix, 1:(num_of_columns - 1) , sep = "_")
    column_names[num_of_columns] <- paste(column_prefix, "OTHER", sep = "_")
    print(column_names)
    sol <- data.frame(m)
    colnames(sol) <- column_names
  } else {
    sol <- str_split(vect, pattern = "\\|")
    print("else")
  }
  return(sol)
}

# applying function on columns

ajker <- split_by_divider(
  tib$Ajánlatkérő.szervezet.neve..adatfrissítés.időpontjában.,
                          num_of_columns = 7, column_prefix = "Ajanlatkero")
ajker_r <- split_by_divider(
  tib$Ajánlatkérő.szervezet.neve..adatfrissítés.időpontjában.)

ajker_id_r <- split_by_divider(tib$Ajánlatkérő.nemzeti.azonosítószáma..adatfrissítés.időpontjában.)

cpv <- split_by_divider(tib$További.CPV.kód.ok.,
  num_of_columns = 16,
  column_prefix = "CPV"
)
cpv_r <- split_by_divider(tib$További.CPV.kód.ok.)


cpv_main <- split_by_divider(tib$Fő.CPV.kód.ok., 
                             num_of_columns = 3, 
                             column_prefix = "Fo_CPV")
cpv_main_r <- split_by_divider(tib$Fő.CPV.kód.ok.)

nuts <- split_by_divider(tib$Teljesítés.helye.NUTS.kód.ok., 
                         num_of_columns = 5,
                         column_prefix = "Telj_helye_NUTS")
nuts_r <- split_by_divider(tib$Teljesítés.helye.NUTS.kód.ok.)

ajtev_r <- split_by_divider(tib$Nyertes.ajánlattevő.neve)
# 3 fölött csak 1600 elem
ajtev <- split_by_divider(tib$Nyertes.ajánlattevő.neve, 
                            num_of_columns = 4, 
                            column_prefix = "nyertes_ajanlattevo")

ajtev_postai_r <- split_by_divider(tib$Nyertes.ajánlattevő.postai.címe)
ajtev_postai <- split_by_divider(tib$Nyertes.ajánlattevő.postai.címe,
                                  num_of_columns = 4,
                                  column_prefix = "nyertes_ajanlattevo_postai_cime")



uniform_names_based_on_id <- function(names, ids) {
  unique_names <- list()
  solution <- data.frame(1:length(names))
  solution$n <- names
  solution$i <- ids
  
  for (row_n in 1:length(ids)) {
    for (id_n in 1:length(ids[[row_n]])) {
      id <- solution$i[[row_n]][[id_n]]
      if (id == "" || id == "-") {
        print("id is empty, next")
        next
      }
      id <- str_replace_all(id, pattern = ",,$|,$", replacement = "") |> str_replace_all(pattern = "(,,|,)\\s?", replacement = " ")
      if (id != solution$i[[row_n]][[id_n]]) {
        solution$i[[row_n]][[id_n]] <- id
        print(paste("id", ids[[row_n]][[id_n]], "replaced with", id))
      }
      name <- names[[row_n]][[id_n]]
      if (!is.null(unique_names[[id]]) && solution$n[[row_n]][[id_n]] != unique_names[[id]]) {
        solution$n[[row_n]][[id_n]] <- unique_names[[id]]
        print(paste("changed", name, "to", unique_names[[id]], sep = "."))
        next
      }
      unique_names[id] <- name
    }
  }
  return(solution)
}

sol <- uniform_names_based_on_id(names = ajker_r, ids = ajker_id_r)


adoszam_df <- tibble(tib$Nyertes.ajánlattevő.adószáma..adóazonosító.jele.)
print(head(adoszam_df))
print("we good")
adoszam_df$eu <- list(NA)
adoszam_df$cegj <- list(NA)
adoszam_df$hun <- list(NA)
adoszam_df$split <- str_split(adoszam_df[[1]], pattern = "\\||,")

for (row_n in 1:length(adoszam_df$split)) {
  row <- adoszam_df$split[[row_n]]
  for (id in row) {
    print(id)
    if (str_starts(id, pattern = "[:upper:]{2}\\d+")) {
      if (is.na(adoszam_df$eu[row_n])) {
        adoszam_df$eu[[row_n]] <- id
      } else {
        adoszam_df$eu[[row_n]] <- c(adoszam_df$eu[[row_n]], id)
      }
    }
    if (str_starts(id, pattern = "[:alpha:]*\\.*\\d{2}-")) {
      if (is.na(adoszam_df$cegj[row_n])) {
        adoszam_df$cegj[[row_n]] <- id
      } else {
        adoszam_df$cegj[[row_n]] <- c(adoszam_df$cegj[[row_n]], id)
      }
    }
    if (str_starts(id, pattern = "(\\d{8}-)|\\d{11}")) {
      if (is.na(adoszam_df$hun[row_n])) {
        adoszam_df$hun[[row_n]] <- id
      } else {
        adoszam_df$hun[[row_n]] <- c(adoszam_df$hun[[row_n]], id)
      }
    }
  }
}

save(adoszam_df, file = "ado1.Rdata")
load(file = "ado1.Rdata")
adoszam_excel <- adoszam_df
adoszam_excel <- hoist(adoszam_excel, hun, ado_hu_1 = 1, ado_hu_2 = 2)
adoszam_excel <- hoist(adoszam_excel, eu, ado_eu_1 = 1, ado_eu_2 = 2)
adoszam_excel$hun_other <- lapply(adoszam_excel$hun, function(x) {ifelse(is.logical(x), "", paste(x, collapse="|"))}) |> unlist()
adoszam_excel$eu_other <- lapply(adoszam_excel$hun, function(x) {ifelse(is.logical(x), "", paste(x, collapse="|"))}) |> unlist()
adoszam_excel$cegj <- lapply(adoszam_excel$cegj, function(x) {ifelse(is.logical(x), "", paste(x, collapse="|"))}) |> unlist()
# drop unneccessary cols

adoszam_excel <- select(adoszam_excel, -c("hun", "split", "tib$Nyertes.ajánlattevő.adószáma..adóazonosító.jele.", "eu"))
write_xlsx(adoszam_excel, path = "adoszam.xlsx")

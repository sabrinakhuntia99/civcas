#' @export
expmat <- function(infile){
  datset <- data.table::fread(infile, header = TRUE)
  datset <- datset[!duplicated(datset[, 1]), ]
  matset <- as.matrix(datset[, -1, with = FALSE])
  rownames(matset) <- unlist(datset[, 1, with = FALSE])
  matset
}

sexcount <- function(infile){
  datset <- data.table::fread(infile, header = TRUE)
  M_count <- dplyr::count(datset, sex == "M")
  F_count <- dplyr::count(datset, sex == "F")
  U_count <- dplyr::count(datset, sex == "U")
  M_count[2,2]
  F_count[2,2]
  U_count[2,2]
  D_sexcount <- data.frame(
    name=c("Male", "Female", "Unknown"),
    value=c(M_count, F_count, U_count)
  )
  barplot(height=D_sexcount$value, names=D_sexcount$name,
          xlab="Sex", 
          ylab="Number of Deaths", 
          main="Death Toll by Sex"
  )
}

deathcount <- function(infile){
  datset <- data.table::fread(infile, header = TRUE)
  aba_count <- dplyr::count(datset, vars = aba)
  exh_count <- dplyr::count(datset, vars = exh)
  hrw_count <- dplyr::count(datset, vars = hrw)
  aba_count[2,2]
  exh_count[2,2]
  hrw_count[2,2]
  D_deathcount <- data.frame(
    name=c("ABA", "Exhumation", "HRW"),
    value=c(aba_count, exh_count, hrw_count)
  )
  barplot(height=D_deathcount$value, names=D_deathcount$name,
          xlab="Death Identification Type", 
          ylab="Death Toll", 
          main="Deaths by Identification Type"
  )
}

deathwt <- function(infile){
  datset <- data.table::fread(infile, header = TRUE)
  whole_count <- dplyr::count(datset, weight = "0.33")
  part_count <- dplyr::count(datset, weight = "1")
  whole_count[2,2]
  part_count[2,2]
  D_wtcount <- data.frame(
    name=c("Complete", "Imputed"),
    value=c(whole_count, part_count)
  )
  barplot(height=D_wtcount$value, names=D_wtcount$name,
          xlab="Weight", 
          ylab="Toll", 
          main="Deaths by Record Date"
  )
}
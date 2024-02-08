#' Read MAIA data in .txt files
#' @name read_maia_txt
#' @description
#' Extraction of data from exported .txt files
#' @author Tjebo
#' @param dir source folder. Default: working directory (check with
#'   getwd())
#' @importFrom readr read_tsv
#' @return Data frame
#' @examples
#' # read_maia_txt()
#' @export

read_maia_txt <- function(dir = getwd()){
ls_files <- paste(dir, list.files(dir), sep = "/")
ls_test <- lapply(ls_files, readLines)

res <- lapply(ls_test, function(x) {
  split(x, cumsum(endsWith(x, ':')| startsWith(x, "ID")))
})

res_rename <- lapply(res, function(x) {
  setNames(lapply(x, `[`, -1), sapply(x, `[`, 1))
  })

res_nocolon <- lapply(res_rename, function(x) {
  names(x) <- tolower(stringr::str_trim(gsub(":", "", names(x))))
  newnames <- unlist(sapply(lu_names, function(y) grep(y, names(x))))
  names(x)[newnames] <- names(newnames)
  x
  })

pt_frame <- lapply(res_nocolon, function(x){
  thresh_mes <- "id\tx_deg\ty_deg\tmesopic"
  thresh_scot <- "id\tx_deg\ty_deg\tcyan\tred\tdiff"

  if(any(names(x) == "threshold")){
    if(any(grepl("mesopic", names(x)))){
    x[["threshold"]] <- c(thresh_mes, x[["threshold"]])
    } else if(any(grepl("scotopic", names(x)))){
      x[["threshold"]] <- c(thresh_scot, x[["threshold"]])
    }
    x[["threshold"]] <-
      readr::read_tsv(paste(x[["threshold"]], collapse = "\n"))
  }
  if(any(names(x) == "reg_pts")){
  x[["reg_pts"]] <-
    readr::read_tsv(paste(x[["reg_pts"]], collapse = "\n"))
  }
  if(any(names(x) == "fix_pts")){
    x[["fix_pts"]] <-
      readr::read_tsv(paste(x[["fix_pts"]], collapse = "\n"))
  }
  x
})
pt_frame
}


 # mydir <- "./data-raw/maia-4013_data"
# read_maia_txt(mydir)



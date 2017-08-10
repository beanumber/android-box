#' read XSPF playlist
#' @param x path to XSPF playlist
#' @importFrom xml2 read_xml as_list
#' @importFrom magrittr %>%
#' @importFrom utils URLdecode
#' @export
#' @examples
#' pg <- read_xspf("~/Desktop/Car Music.xspf")

read_xspf <- function(x, ...) {
  if(file.exists(x)) {
    pg <- xml2::read_xml(x)
  }

  playlist <- xml2::as_list(pg)

  # get all the <record>s
  # xml_find_all(pg, "//location")
  # vals <- trimws(xml_text(recs))

  get_location <- function(y) {
    y$location[[1]]
  }

  get_location(playlist[[1]][[1]])
  locs <- lapply(playlist[[1]], get_location) %>%
    unlist()

  # clean up HTML characters
  new_locs <- sapply(locs, utils::URLdecode) %>%
    gsub("file://", "", x = .)

  return(new_locs)
}

#' sync files to phone
#' @param files character vector of file paths
#' @param dir directory to put the files
#' @export
#' @examples
#'
#' pg <- read_xspf("~/Desktop/Car Music.xspf")
#' sync(pg)

sync <- function(files, dir = "~/Desktop/Phone_Music/", ...) {
  dir.create(dir, recursive = TRUE)
  file.copy(from = files, to = dir, recursive = TRUE)

  # get Samsung Music folder
  phone_dir <- list.dirs("/run/user/1000/gvfs", recursive = FALSE)
  music_dir <- file.path(phone_dir, "Phone", "Music")
  system(paste("rsync -nvz", dir, music_dir))
}

# cmds <- paste0("cp '", new_locs, "' ~/Desktop/music/")
# lapply(cmds, system)

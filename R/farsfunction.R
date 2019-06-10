#' fars_read
#'
#' This function permit us to read the data that is inside the file.

#' @param filename A specific file: accident_XXXX.cvs.bz2
#' @details If it fails to find a specified file, it will show an error, playing to the user
#' that the file doesn't exist. The function assumes that the file is in the current working
#' directory.
#' @return return a tibble (a tidyverse version of the dataframe) of \code{filename} (fars events)
#' @example add fars_read("accident_2014.csv.bz2")
#' @importFrom dplyr filter
#' @importFrom readr read_cvs
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'
#' make_filename
#'
#' This function has the aim to return the data or the formatted combination of texts and variables
#' values that are comprised in any of this file "accident_%d.csv.bz2". This function lets you
#' return a string only writing the year that the file is labeled.
#'
#' @param year A string of four numbers.
#' @return A string in which there is a the formatted combination of texts and variables values.
#' @note Entered year is first changes into integer and only then used to generate
#' a filename. Non-numbers will shift string into NA.
#' @example Add make_filename (2013)
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This function lets us read a list of data.frames (fars events) by means of a list of years
#' @param years an integer vector that specify what data should be imported into R
#' @return A list of tibbles with two columns (years and month), which is derived from what we
#'  wrote in the function (\code{years}).
#'  @details If the data for the writting is not appropriate or available, the function will show a
#' list with NULL value.
#' @example
#'  \dontrun{fars_read_years(c(2013,3014))}
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#'
#' This function can brings us an overall summary of fars, after we write the years that we want to
#' analyze.
#' @param years an integer vector that specify what data should be imported into R
#' @return A general summary of the tibble, which are organized by year and month.
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#' @example
#'  \dontrun{fars_summarize_yeras(c(2013,2015))}
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' create a map of fars events from a specific state.

#' @param state.num is the number of the state.
#' @param year A string of four numbers.
#' @return Plots a map of fars events, according to the state of interest.
#' @details If the state number is wrong, state number, the function will cancel and show an error.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{fars_map_state(20,2013)}
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

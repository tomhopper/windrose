#' @name print
#' @title Prints a windrose object to console
#' @description Prints a windrose object to the console.
#' @usage print(data_rose)
#' @param data (required) a windrose object
print.windrose <-
function(x) {
  hidden <- attr(x, "hidden")
  print(x[!names(x) %in% hidden])
}

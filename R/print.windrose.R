print.windrose <-
function(x) {
  hidden <- attr(x, "hidden")
  print(x[!names(x) %in% hidden])
}

#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

# Avoid R CMD check note when using NSE of . in magrittr pipes
if(getRversion() >= "2.15.1")  utils::globalVariables(".")
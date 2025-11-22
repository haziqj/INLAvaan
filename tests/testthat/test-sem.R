# library(lavaan)
# mod <- "
#   ind60 =~ x1 + x2 + x3
#   dem60 =~ y1 + y2 + y3 + y4
#   dem65 =~ y5 + y6 + y7 + y8
#
#   dem60 ~ ind60
#   dem65 ~ ind60
#   dem60 ~~ dem65
#
#   y1 ~~ y5
#   y2 ~~ y4 + y6
#   y3 ~~ y7
#   y4 ~~ y8
#   y6 ~~ y8
# "
# fit <- sem(mod, PoliticalDemocracy)
# pt <- as_tibble(inlavaan(mod, PoliticalDemocracy, "sem"))
#
# pt |>
#   mutate(across(g:ginv_prime, function(x) gsub("\\\n", "", as.character(x)))) |>
#   write_csv(file = "pt.csv")

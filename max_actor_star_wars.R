install.packages("rvest", dependencies=TRUE)
require(rvest)
movie1 <- read_html("https://www.imdb.com/title/tt0120915/?ref_=nv_sr_7")
movie2 <- read_html("https://www.imdb.com/title/tt0121765/?ref_=nv_sr_1")
movie3 <- read_html("https://www.imdb.com/title/tt0121766/?ref_=nv_sr_8")
movie4 <- read_html("https://www.imdb.com/title/tt0076759/?ref_=nv_sr_2")
movie5 <- read_html("https://www.imdb.com/title/tt0080684/?ref_=nv_sr_6")
movie6 <- read_html("https://www.imdb.com/title/tt0086190/?ref_=nv_sr_7")
movie7 <- read_html("https://www.imdb.com/title/tt2488496/?ref_=nv_sr_2")
movie8 <- read_html("https://www.imdb.com/title/tt2527336/?ref_=fn_al_tt_5")


cast1 <- movie1 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast1

cast2 <- movie2 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast2

cast3 <- movie3 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast3

cast4 <- movie4 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast4

cast5 <- movie5 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast5

cast6 <- movie6 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast6

cast7 <- movie7 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast7

cast8 <- movie8 %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast8

cast <- c(cast1, cast2, cast3, cast4, cast5, cast6, cast7, cast8)

table(cast)[table(cast) == max(table(cast))]

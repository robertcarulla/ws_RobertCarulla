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

movies <- list(movie1, movie2, movie3, movie4, movie5, movie6, movie7, movie8)

cast <- c()
for (i in 1:8){
  f <- movies[[i]] %>%
    html_nodes("#titleCast .itemprop span") %>%
    html_text()
  cast <-c(cast, f) 
}

table(cast)[table(cast) == max(table(cast))]


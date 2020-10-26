library(rvest)
library(xml2)
library(forcats)
library(stringr)

webpage_url <- "https://en.wikipedia.org/wiki/List_of_jōyō_kanji"
webpage <- xml2::read_html(webpage_url)

joyo.raw <- rvest::html_table(webpage)[[2]] %>% 
  tibble::as_tibble(.name_repair = "unique")

splt <- str_locate(joyo.raw$Readings,"[a-zōū]")[,1] - 1
read_jp <- str_sub(joyo.raw$Readings, 1, splt) %>% str_trim(side="both")
read_en <- str_sub(joyo.raw$Readings, splt + 1, str_length(joyo$Readings)) %>% str_trim(side="both")

joyo <- joyo.raw %>% 
  mutate(Grade=as.factor(Grade),
         Radical=as.factor(Radical),
         Strokes=factor(Strokes, levels=1:29),
         New=str_replace(New, "\\s*(.\\d.)",""),
         Old=str_replace(Old, "\\s*(.\\d.)",""),
         Old=ifelse(as.character(Old)!="", Old, NA),
         read_jp = read_jp,
         read_en = read_en) %>%
  select(ref=`#`, kanji=New, radical=Radical, strokes=Strokes, grade=Grade,
         meaning=`English meaning`, read_jp, read_en)


split_jp <- "、"
split_en <- ", "

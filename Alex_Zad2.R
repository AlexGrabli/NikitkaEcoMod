data = read_csv("eddypro.csv", skip = 1, na = "-9999")
#Александров Н.А. 125 ПАЭ 
#Задание 2
#создайте модель множественной линейной регрессии дневных потоков паров воды за весенний период 2013 года по данным измерений методом турбулентной пульсации

library(readr)
tbl = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"),  comment=c("["))
tbl
tbl=tbl[-1,]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl
tbl = tbl %>% mutate_if(is.character, factor)
tbl
names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")%>%
  tbl
glimpse(tbl)
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
cor_td = cor(tbl_numeric)
cor_td
cor_td = cor(drop_na(tbl_numeric))
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
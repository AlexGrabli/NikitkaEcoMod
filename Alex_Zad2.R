#Александров Н.А. 125 ПАЭ 
#Задание 2
#создайте модель множественной линейной регрессии дневных потоков паров воды за весенний период 2013 года 
#по данным измерений методом турбулентной пульсации

#Подключаем пакеты
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")  
library("tibble")
library("readr") 
library("rnoaa")
library("lubridate")

#Считываем данные из файла; Пропускаем первую строчку, заменяем текстовые NA,
#заменяем все не числовые значения на NA, пропускаем строчки с "["
data = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"),  comment=c("["))
#удаляем ненужный пустой столбец
data = select(data, -(roll))
#Преобразуем строковые значения в факторные
data = data %>% mutate_if(is.character, factor)
#Заменяем символы в названии столбцов на допустимые для переменных названия
names(data) = names(data) %>% 
  str_replace_all("[!]", "_exclam_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")
#Превращаем столбцы таблицы в виде векторов для проверки
glimpse(data)
#Убераем NA
data = drop_na(data)
#Отфильтруем данные для летнего периода (1 марта  - 60 день, 31 мая - 151 день)
data = filter(data, DOY >= 60 & DOY <= 151)
#Отфильтруем данные для дневного периода
data = filter(data, daytime==TRUE)

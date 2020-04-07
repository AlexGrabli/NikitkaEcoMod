#Александров Никита - для региона 32 (Брянск) рассчитайте урожайность пшеницы в 1999 году,взяв для рассчета средние
#суммы активных температур за указанный год, с 13 ближайших метеостанций но рассчитав колонку di самостоятельно
#как долю месяца, когда среднедневные температуры были выше 7 градусов, но учитывая, что посев не может начаться 
#раньше середины апреля, а вегетация составляет 4 месяца.

#создадим векторы с данными для расчета
ai = c(0.00,0.00,0.00,32.11,26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bi = c(0.00,0.00,0.00,11.30,9.26,9.03,8.16,6.59,5.73,4.87,0.00,0.00)
di = c(0.00,0.00,0.00,0.33,1.00,1.00,1.00,0.32,0.00,0.00,0.00,0.00)
Kf = 300
Qj = 1600
Lj = 2.2
Ej = 25
y = 1.0

bryansk = data.frame(id = "BRYANSK", latitude = 53.2446861,  longitude = 34.3657761)
bryansk_around = meteo_nearby_stations(lat_lon_df = bryansk, station_data = station_data,
                                       limit = 13, var = c("PRCP", "TAVG"),
                                       year_min = 1999, year_max = 1999)
#bryansk_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Брянска.
all_data = tibble()
for (v in 1:13)
{
  bryansk_id = bryansk_around[["BRYANSK"]][["id"]][v]
  #
  data = meteo_tidy_ghcnd(stationid = bryansk_id,
                          var="TAVG",
                          date_min="1999-01-01",
                          date_max="1999-12-31")
  all_data = bind_rows(all_data, data)
}
write.csv(all_data,file="alldata.csv")
#произведем обработку полученных данных
all_bryansk = all_data %>% 
  mutate(year = year(date), month = month(date), day = day(date)) %>% 
  group_by(month, id) %>% 
  mutate(tavg=tavg/10) %>% filter (tavg>5) %>% 
  #
  summarise(sum = sum(tavg,na.rm = TRUE)) %>% 
  #
  #
  group_by(month) %>% 
  summarise(S = mean(sum,na.rm = TRUE)) %>% 
  #
  mutate(F = ((ai + bi * y * S * di) * Kf) / (Qj * Lj * (100 - Ej)))
#
Yield = sum(all_bryansk$F); Yield
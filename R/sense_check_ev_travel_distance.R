#sense check EV electricity consumption for average kilometers driven

#NSW ACT BEV fleet

fleet <- 132540

average_km <- 10.69 * 1000

average_kWh_per_km <- 0.23 #csiro 2022

EV_consumption_gwh <- fleet * average_km * average_kWh_per_km / 1e6 

work_book_consumption <- 60 + 116 + 76


large <-  27907 * 10.69 * 1000 * 0.265

medium <-  56036 * 10.69 * 1000 * 0.23

small <-  48597 * 10.69 * 1000 * 0.168


total <- (large + medium + small) / 1e6

#it appears that the assumed travel distance is lower in AEMO than ABS average. Either that or the or the consumption rates used are wrong...
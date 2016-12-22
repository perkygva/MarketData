source('G:/GENEVA/Code/R/Allan/Fun/Load.R')

prod1 <- c("S", "C", "W")
prod2 <- c("KW", "CT", "KC", "DF", "CC", "QC")
contracts <- c("H", "K", "N", "U", "Z")
fields <- c("open_int", "fut_aggte_open_int", "aggregate_call_open_int", "aggregate_put_open_int")

con = blpConnect()

bbg = bloomy(paste0(prod1, " A Comdty"))
names(bbg) = c("S", "C", "W")

blpDisconnect(con)

foreach(i %in% i:length(bbg))%do%{
  df <- data.frame(list[[i]])
  setkey(df, date)
  setnames(df, colnames)
}
  

ggplot(bbg$S, aes(date, fut_aggte_open_int))+geom_line()+ geom_line(aes(bbg$C$fut_aggte_open_int), col = "red")

c("SBh7 Comdty", "QWh7 Comdty", "KCH7 Comdty","DFF7 Comdty","CCH7 Comdty","QCh7 Comdty","CTH7 Comdty","JOH7 Comdty","C h7 Comdty","S F7 Comdty",
  "SMh7 Comdty","BOh7 Comdty","W h7 Comdty","CAh7 Comdty", "LHh7 Comdty", "LCV6 Comdty")
fields = "HIST_CALL_IMP_VOL"
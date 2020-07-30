library(tidyverse)
library(stringr)
library(TSA)
chat = read.csv("C:/Users/fede2/OneDrive - University of Miami/MSBA/WhatsApp Chat with +1 (954) 937-4260.txt")%>%
  rename(Date = X12.5.16,
         Entry = X20.03...Messages.to.this.chat.and.calls.are.now.secured.with.end.to.end.encryption..Tap.for.more.info.)%>%
  filter(str_detect(Entry, 'miles'))%>%
  mutate(Entry = gsub('miles/', ' miles /', Entry))%>%
  separate(col = Entry, into = c('old','Step1'), sep = 'Oldani: ', remove = FALSE)%>%
  separate(col = Step1, into = c('Miles', 'Step2'), sep = 'miles / \\$')%>%
  mutate(Miles = gsub(' ', '', Miles))%>%
  separate(col = Step2, into = c('Cost', 'Gallons'), sep = ' per ')%>%
  mutate(Miles = as.numeric(Miles),
         Date = as.Date(Date, format = '%m/%d/%y'),
         Cost = as.numeric(gsub(' ', '', Cost)),
         Gallons = as.numeric(gsub('gal', '', Gallons)),
         Distance = as.numeric(c(0, Miles[2:length(Miles)] - Miles[1:(length(Miles) - 1)])),
         CostperGallon = as.numeric(round(Cost / Gallons, 2)),
         MilesperGallon = as.numeric(round(c(0, Distance[2:length(Distance)] / Gallons[1:(length(Distance) - 1)]), 2)))%>%
  select(-Entry,-old)%>%
  write.csv("C:/Users/fede2/OneDrive - University of Miami/MSBA/GasTrackingCLEAN.csv")
d = read.csv("C:/Users/fede2/OneDrive - University of Miami/MSBA/GasTrackingCLEAN.csv")


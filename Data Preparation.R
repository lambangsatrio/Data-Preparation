# # Title      : Homework Data Preparation
# Author      : Lambang Satrio Nuli Raharjo

# Install Package 'readxl' untuk membaca data input bertipe .xls dan .xlsx
install.packages('readxl')

# Load package 'readxl'
library('readxl')
library(tidyverse)

# Membaca dataset bertipe excel
getwd()
setwd("D:/TRAINING MBA 2019/DAY 11- 210919 - Preparing the data for deeper analysis/")

df_online_retail = read_excel("Online_Retail.xlsx")

# Tampilkan 6 data teratas 
head(df_online_retail)

# Tampilkan 6 data terbawah
tail(df_online_retail)

# Tampilkan summary data
summary(df_online_retail)

# Tampilkan jumlah missing data ke dalam plot
library(DataExplorer)
plot_missing(df_online_retail)
# Terdapat 24.93% missing data pada field CustomerID

# Tampilkan struktur data
str(df_online_retail)

# Drop data dengan CustomerID = Na
df_online_retail_drop = df_online_retail[!is.na(df_online_retail$CustomerID),] 

# Cek Missing data menggunakan plot
plot_missing(df_online_retail_drop)
#Sudah tidak ada missing data di field CustomerID

summary(df_online_retail_drop)

# Membuat tabel dengan 2 variabel: 
# CustomerID | frequency
# frequency adalah berapa kali satu orang membeli barang
frequency = df_online_retail_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo))
head(frequency)


# Membuat tabel dengan 2 variabel:
# CustomerID | monetary
# monetary adalah berapa jumlah uang yang dibelanjakan oleh masing-masing orang
monetary = df_online_retail_drop %>% group_by(CustomerID) %>% summarise(monetary = sum(UnitPrice*Quantity))
head(monetary)

# Menambahkan satu kolom di tabel df_online_retail_drop bernama 'recency'
# recency berisi berapa hari sejak pembelian terakhir customer ke tanggal 31 Desember 2011
recency = df_online_retail_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>% filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate, ymd('2011-12-31'))))/86400)
head(recency)

library('DataExplorer')
# Join ketiganya
df_rfm = recency %>% left_join(frequency, by = 'CustomerID') %>% left_join(monetary, by = 'CustomerID')

# Tampilkan hanya CustomerID, Recency, Frequency, dan Monetary sesuai soal:
df_result = df_rfm %>% select(CustomerID, recency, frequency, monetary)
head(df_result)

# write result
write.csv(df_result, "D:/TRAINING MBA 2019/DAY 11- 210919 - Preparing the data for deeper analysis/online_retail_result_lambang.csv", quote=F, row.names = F)


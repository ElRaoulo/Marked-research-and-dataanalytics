library(tidyverse)

#import data
data <- read_csv("pretest_data.csv")


#filter for outfits only
data_outfits <- data[, grepl("_outfits", names(data))]
#filter for outfits that inspect the consistency
data_outfits_konsistenz <- data_outfits[, grepl("Konsistenz", names(data_outfits))]

#function that averages over all three consistency questions
reduce_columns <- function(df) {
  df %>%
    mutate(
      HipHop1 = rowMeans(select(., 1:3)),
      Hippie1 = rowMeans(select(., 4:6)),
      Min1 = rowMeans(select(., 7:9)),
      RelativeSize_MinXHippie2 = rowMeans(select(., 10:12)),
      Abgrenzung_HipHopXMin2 = rowMeans(select(., 13:15)),
      Zentralität_MinXHippie2 = rowMeans(select(., 16:18)),
      HipHop2 = rowMeans(select(., 19:21)),
      Hippie2 = rowMeans(select(., 22:24)),
      Min2 = rowMeans(select(., 25:27)),
      RelativeSize_HippieXHipHop1 = rowMeans(select(., 28:30)),
      Abgrenzung_HipHopXMinXHippie1 = rowMeans(select(., 31:33)),
      Zentralität_MinXHippieXHipHop = rowMeans(select(., 34:36))
    )
}

konsistenz <- reduce_columns(data_outfits_konsistenz)[,37:48]
#transpose to get a better look
konsistenz_mean <- konsistenz %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  t()

#do the same with liking
data_outfits_liking <- data_outfits[, grepl("Liking", names(data_outfits))]
  
liking_mean <- reduce_columns(data_outfits_liking)[,37:48] %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  t()

#overview of Konsistenz and Liking of the outfits
konsistenzXliking <- merge(konsistenz_mean, liking_mean, by = "row.names", all = TRUE)




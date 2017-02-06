library(readr)

train = read_csv("Gun_Point_TRAIN", col_names = FALSE)
train = as.data.frame(train)
test = read_csv("Gun_Point_TEST", col_names = FALSE)
test = as.data.frame(test)

gunpoint = rbind(train, test)
save(gunpoint, file = "gunpoint.RData")



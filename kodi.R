x <- c(1,2,3)
y <- c("a", "b", "c")
df <- data.frame(x, y)
print(df)
library(readr)
library(dplyr)
library(lubridate) # pakki med follum fyrir vinnslu med dagsetningar (date eda datetime breytur)
library(ggplot2)
data <- read_csv("./data/vedurgogn.csv")

mean(data$stöð, na.rm = TRUE)

data %>%
  summarise(meanSolstundir = mean(sun, na.rm = TRUE),
            sdSolstundir = sd(sun, na.rm = TRUE)
            )

data %>%
  mutate(litur = sample(c("gulur","raudur","graen"), size = nrow(data), replace = TRUE)) %>%
    group_by(litur) %>%
      summarise(meant = mean(t, na.rm = TRUE))

library(ggplot2)
    
data %>%
  ggplot(aes(x = sun, y = r)) + 
  geom_point()

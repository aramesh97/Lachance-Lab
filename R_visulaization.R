library("readr")
library("ggplot2")
data <- read.table(file = "/Users/ashikaramesh/Documents/LachanceLab/analysis.txt",header = TRUE ,fill = TRUE)
summary(data)
breaks <- c(0,0.005,0.01,0.05,0.2,0.5)
tags <- c("[0-0.005)","[0.005-0.01)", "[0.01-0.05)", "[0.05-0.2)", "[0.2-0.5)")
vgroup <- as_tibble(data) %>% 
  mutate(tag = case_when(
    MAF < 0.005 ~ tags[1],
    MAF >= 0.005 & MAF < 0.01 ~ tags[2],
    MAF >= 0.01 & MAF < 0.05 ~ tags[3],
    MAF >= 0.05 & MAF< 0.2 ~ tags[4],
    MAF >= 0.2 & MAF < 0.5 ~ tags[5],
  ))
summary(vgroup)
vgroup$tag <- factor(vgroup$tag,
                     levels = tags,
                     ordered = FALSE)
summary(vgroup$tag)
ggplot(data = vgroup, mapping = aes(x=tag,y=Rsq)) + 
  geom_jitter(aes(color='blue'),alpha=0.2) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3) + 
  labs(x='Minor allele frequency') +
  guides(color=FALSE) +
  theme_minimal() 

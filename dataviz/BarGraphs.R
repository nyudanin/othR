library(ggplot2)
library(gridExtra)
library(reshape2)

FACSdata <- read.delim("~/Desktop/3-1A.txt", stringsAsFactors=FALSE)
ColData <- read.delim("~/Desktop/ColData.txt")
ColNames <- read.delim("~/Desktop/ColNames.txt", header=FALSE)

colnames(FACSdata) <- ColNames[[1]]
FACSdata <- cbind.data.frame(ColData, FACSdata)
rm(ColData)
rm(ColNames)

FACSdata <- melt(FACSdata, 
                 id.vars = c("Time","Replicate","Experiment","Tissue","Diet" ),
                 variable.name = "Population", 
                 value.name = "Frequency")

FACSdata$Time <- as.factor(FACSdata$Time)
FACSdata$Replicate <- as.factor(FACSdata$Replicate)

cairo_pdf(filename="3-1A Frequencies.pdf", w=30, h=8)
ggplot(data=FACSdata, aes(x=Time, y=Frequency, color=Diet, fill=Diet)) +
  facet_wrap( Tissue ~ Population, scales="free_y", nrow=2) +
  stat_summary (fun.y = "mean", geom = "bar", position = position_dodge()) +
  scale_fill_manual(values = c(Control= "#707070", Fat="#269040")) +
  scale_color_manual(values = c(Control= "#707070", Fat="#269040")) +
  geom_jitter(position=position_jitterdodge(0.9), shape=21, fill="white") +
  ggtitle("3-1A Frequencies") +
  theme(legend.position='none') +
  labs(title="3-1A Frequencies", x="Time (weeks)", y = "% CD45+")
dev.off()


  



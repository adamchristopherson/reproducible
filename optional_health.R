setwd(dir="~/Desktop/R_class/reproducible/")
filepath = "payments.csv"
#sets directory and file

## Plot 1
df <- read.csv("payments.csv")
df_NY <- subset(df, Provider.State == "NY")
#reads in file and subsets for NY state

pdf(file="plot1.pdf")
with(df_NY, plot(log10(Average.Covered.Charges), log10(Average.Total.Payments), 
                 ylab = "log10(Mean Total Payments)", 
                 xlab = "log10(Mean Covered Charges)", 
                 main="Relationship between Mean Covered Charges and Mean Total 
                 Payments in New York"))
#opens graphics device and makes plot (log to spread points out in plot)
abline(lm(log10(Average.Total.Payments)~log10(Average.Covered.Charges), df_NY), col="red")
#adds linear regression line
dev.off()
#close device

## Plot 2
levels(df$DRG.Definition) <- c("194", "292", "392", "641", "690", "871")
#renames medical conditions to make more manageable
statenames <- unique(df$Provider.State)
diseases <- unique(df$DRG.Definition)
#extracts unique instances of state and condition to store for looping

pdf(file="plot2.pdf")
par(mar = rep(2, 4), oma=c(4,4,4,2), mfrow=c(6,6))
#opens graphic device and sets plotting parameters
for (i in statenames){
    for (j in diseases){
        with(subset(df, Provider.State == i & DRG.Definition == j),
             plot(log10(Average.Covered.Charges),log10(Average.Total.Payments), 
                  main=paste(i,j), 
                  ylim=range(log10(df$Average.Total.Payments)),
                  xlim = range(log10(df$Average.Covered.Charges)))
        )
        abline(lm(log10(Average.Total.Payments)~log10(Average.Covered.Charges),
                  subset(df, Provider.State == i & DRG.Definition == j) ), 
               col="red")
    } 
}
#loops over the states and diseases, subsetting by each
#the x and y axes are rescaled to make the scales are common through all panels
#chose log-log so data are more in center of the plot
#draws linear regression line over the points in red for easy of visualization
mtext("Trends between Mean Covered Charges and Mean Total 
      Payments by Medical Condition and State", side = 3, line = 0, 
      outer = TRUE, font=2)
mtext("log10(Mean Covered Charges)", side = 1, line=1, outer = TRUE)
mtext("log10(Mean Total Payments)", side = 2, line = 2, outer = TRUE)
#adds an overall title, and overall x and y labels
dev.off()
#close graphics device
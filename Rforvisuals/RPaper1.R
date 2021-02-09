#==This file prsents R graphs for paper 1.......

#https://cran.r-project.org/web/packages/wbstats/vignettes/Using_the_wbstats_package.html

##Save everything in R
save.image(file="Regional.RData")
getwd()

library(wbstats)
library(ggplot2)
library(lubridate)

library(readxl)
PlotAsiaPeper1 <- read_excel("~/Dropbox/ThesisResource/Paper1_Final/Stata/PlotAsiaPeper1.xls")
View(PlotAsiaPeper1)

oil_data <- wb(indicator = c("CRUDE_DUBAI", "CRUDE_BRENT", "CRUDE_WTI", "CRUDE_PETRO"),
               startdate = "2012M01", enddate = "2014M12", freq = "M", POSIXct = TRUE)

ggplot(oil_data, aes(x = date_ct, y = value, colour = indicator)) + geom_line(size = 1) +
  labs(title = "Crude Oil Price Comparisons", x = "Date", y = "US Dollars")


##My plot from the data PlotAsiaPeper1, which comes from paper 1 stata file
p1 <- ggplot(PlotAsiaPeper1, aes(x = year, y = FD, colour = code)) + geom_line(size = 1) +
  theme(legend.position="none") +
  labs(title = "Financial Development Index", x = " ", y = "")

#gdp per capita growth
p2 <- ggplot(PlotAsiaPeper1, aes(x = year, y = gdppcg, colour = code)) + geom_line(size = 1) +
  theme(legend.position="none") +
  labs(title = "GDP per capita growth", x = " ", y = "")

#Household consumpiton per capita
p3 <- ggplot(PlotAsiaPeper1, aes(x = year, y = lnhhfc, colour = code)) + geom_line(size = 1) +
  theme(legend.position="none") +
  labs(title = "Household Final Consumption", x = " ", y = "")

#Poverty Headcount Ratio, by subsetting and excluding missing values
p4 <- ggplot(data=subset(PlotAsiaPeper1, !is.na(lnphcr)), aes(x = year, y = lnphcr, colour = code)) + geom_line(size = 1) +
   theme(legend.position="none") +
   labs(title = "Poverty Headcount Ratio", x = " ", y = "" )

#Poverty Gap, by subsetting and excluding missing values
p5 <- ggplot(data=subset(PlotAsiaPeper1, !is.na(lnpgap)), aes(x = year, y = lnpgap, colour = code)) + geom_line(size = 1) +
  theme(legend.position="none") +
  labs(title = "Poverty Gap", x = "Year", y = "")

#Poverty Gap, by subsetting and excluding missing values
p6 <- ggplot(data=subset(PlotAsiaPeper1, !is.na(lngini)), aes(x = year, y = lngini, colour = code)) + geom_line(size = 1) +
  theme(legend.title=element_blank()) +
  labs(title = "Gini Idex", x = "Year", y = "")

p6 <- p6 + theme(legend.position="bottom", legend.direction="horizontal")
p6
library(gridExtra)
##Creating a function for a  common legend.....
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# 2. Save the legend
#+++++++++++++++++++++++
legend <- get_legend(p6)
# 3. Remove the legend from the plot
#+++++++++++++++++++++++
p6 <- p6 + theme(legend.position="none")

# 4. Create a blank plot
library(cowplot)
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
#http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
g <- grid.arrange(p1, p2, p3, p4,p5,p6, blankPlot,legend, 
             ncol= 2,  nrow = 4, 
             widths = c(2.5, 2.5), heights = c(5, 5, 5, 0.5))

ggsave(file="whatever.pdf", g, width = 9, height = 10) #saves g


#OKAY NOW NEXT PLOT
p11 <- ggplot(PlotAsiaPeper1, aes(x = FD, y = gdppc, colour = code)) + geom_line(size = 1) +
  theme(legend.position="none") +
  labs(title = "", x = " ", y = "Financial Development Index")


p11 <- qplot(FD, lngdppc, data = PlotAsiaPeper1, colour = code, shape = code)
      

p11 + geom_point(aes(color=code, shape=code)) + 
      geom_smooth(aes(color=code, shape=code), 
      method=lm, se=FALSE, fullrange=TRUE)

ggplot(PlotAsiaPeper1, aes(x=FD, y=lngdppc, color=country, shape=country)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


# One ellipse arround all points
ggplot(PlotAsiaPeper1, aes(FD, lngdppc))+
  geom_point()+
  stat_ellipse() +
  labs(title="GDP per capita",
  x="", y = "")

# Ellipse, FD & GDPPC
p11 <- ggplot(PlotAsiaPeper1, aes(FD, lngdppc, color = country)) +
     geom_point() +
     stat_ellipse() +
     theme(legend.position="none") +
     labs(title="", x="", y = "GDP per capita")
     
# Ellipse, FD & hhfcon
p22 <- ggplot(PlotAsiaPeper1, aes(FD, lnhhfc, color = country)) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position="none") +
  labs(title="", x="", y = "Household Final Consumption")

# Ellipse, FD & phcr
p33 <- ggplot(PlotAsiaPeper1, aes(FD, lnphcr, color = country)) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position="none") +
  labs(title="", x="Financial Development Index", y = "Poverty Headcount Ratio")

# Ellipse, FD & lngini
p44 <- ggplot(PlotAsiaPeper1, aes(FD, lngini, color = country)) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position="bottom") + theme(legend.title=element_blank()) +
  labs(title="", x="Financial Development Index", y = "Gini Idex")

# 2. Save the legend
#+++++++++++++++++++++++
legend <- get_legend(p44)
# 3. Remove the legend from the plot
#+++++++++++++++++++++++
p44 <- p44 + theme(legend.position="none")

# 4. Create a blank plot
library(cowplot)
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
#http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
g2 <- grid.arrange(p11, p22, p33, p44, blankPlot,legend, 
                  ncol= 2,  nrow = 3, 
                  widths = c(2.5, 2.5), heights = c(5, 5, 1))

ggsave(file="whatever2.pdf", g2, width = 9, height = 10) #saves g


##Scatter plot for Asia (Mean values)
library(haven)
mean <- read_dta("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/Paper1_Final/Rforvisuals/mean.dta")
View(mean)

#first example
px <- ggplot(mean, aes(lncredit, lngdppc, color=country)) + 
  geom_point(size=4)
px

px <- px + facet_wrap(~ country, scales = 'free')

#second exmple
gg <- ggplot(mean, aes(x=lncredit, y=lngdppc, color=country)) + geom_point() + labs(title="Scatterplot", x="Carat", y="Price")  # add axis lables and plot title.
gg

gg + facet_wrap( ~ country, ncol=3)  # columns defined by 'cut'
gg + facet_wrap(color ~ country)  # row: color, column: cut

#another plot
p <- ggplot(mean, aes(lncredit,lngdppc, col = country )) + geom_point()+
  facet_wrap(~ country, scales = 'free') + geom_smooth(method = 'lm', se = FALSE) +
  theme(legend.position="none") +
  labs(title="", x="Domestic Credit to Private Sector", y="Economic Growth")  # add axis lables and plot title.

ggsave(file="whatever.pdf", p, width = 13, height = 10) #saves g


##Now import data for paper 1 for Pakistan only

library(haven)
final_pak <- read_dta("~/Dropbox/ThesisResource/Paper1_Final/Stata/final_pak.dta")
final_pak <- read_dta("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/Paper1_Final/Rforvisuals/final_pak.dta")

ggplot(economics) + geom_line(aes(x=date, y=pce)) + geom_line(aes(x=date, y=unemploy)) + scale_color_discrete(name="Legend") + labs(title="Economics") # plot multiple time series using 'geom_line's
ggplot(final_pak) + geom_line(aes(x=year, y=cps)) + geom_line(aes(x=year, y=inf)) + scale_color_discrete(name="Legend") + labs(title="Financial Development") # plot multiple time series using 'geom_line's

# Approach 2: 
library(reshape2) 
library(lubridate)
library(ggfortify)

##My data example
library(readxl)
rPakistan <- read_excel("D:/Utilisateurs/e0g411m05t7/Dropbox/ThesisResource/Paper1_Final/Rforvisuals/rPakistan.xls",sheet = "Feuil1")

p <- ggplot(rPakistan, aes(year,value, color = Indicator)) +
  geom_line() +
  theme(legend.position="none") +
  labs(title="", x="Year", y = " ")
#title="Evolution of Financial & Macroeconomic Indicators in Pakistan"
p + facet_wrap(~ Indicator)

p <- p + facet_wrap(~ Indicator, scales = 'free')
p
ggsave(file="whatever.pdf", p, width = 9, height = 10) #saves g

##Another mehtod to convert stand alon time series data to make ggplot time sereis

library(faraway)
data(divusa)
head(divusa)
library(ggplot2)
library(reshape2)

divusa = melt(divusa, id.vars = "year")
colnames(divusa)[2] = "category"
head(divusa)

p = ggplot(divusa, aes(year,value, color = category)) +
  geom_line()
p

##Plotting Structural Breaksss
library(urca)

gdppc1 <- final_pak[, c("gdppc","year")]
za.gdppc <- ur.za(gdppc1$gdppc, model = "both", lag = 3)
plot(za.gdppc)
summary(za.qbmony)

#modified code for za plot
plot.ur.za <- function (Time, x, ...) 
{
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 1))
  yvals <- sort(c(x@cval, x@tstats))
  #xvals <- pretty(1:n)
  plot.ts(Time, x@tstats, main = "Zivot and Andrews Unit Root Test", 
          ylab = "t-statistics for lagged endogenous variable", type="l",
          ylim = c(min(yvals), max(yvals)), xy.labels=F, xy.lines=T)
  abline(h = x@cval, col = c("red", "blue", "seagreen"))
  if (x@teststat < x@cval[3]) {
    abline(v = Time[x@bpoint], col = "red", lty = 2)
  }
  mtext(paste("Model type:", x@model, sep = " "), side = 1, 
        line = 4)
  n <- length(Time)
  legend(x = Time[n], y = max(yvals), c("1% c.v.", "2.5% c.v.", 
                                        "5% c.v."), col = c("red", "blue", "seagreen"), xjust = 1, 
         yjust = 1, lty = 1, horiz = TRUE, cex = 0.66, bty = "n")
}

##Now replacing years on x-axis
yrs <- gdppc1$year[-length(gdppc1$year)]
plot.ur.za(Time=yrs, x=za.gdppc)


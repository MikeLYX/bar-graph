# 图0
library(ISLR) 
data(NCI60) # NCI 60 Data
library(RColorBrewer)
dat <- as.data.frame(table(NCI60$labs))
order_dat <- dat[order(dat$Freq,decreasing = TRUE),]
num <- order_dat[,2]
names(num) <- order_dat[,1]
op <- par(mar=c(4,8,2,1))
barplot(num, col = brewer.pal(12,"Set3"), axes = FALSE, border=NA, axisnames=FALSE,
        horiz=TRUE,xlab="numbers",ylab="",legend.text = TRUE)		
xlabs <- seq(0, 9, by = 1)
ylabs <- order_dat[,1]		
axis(1, labels = xlabs, at = xlabs, las = 1) # x-axis
axis(2, labels = ylabs, at = seq(14), las = 1,lwd =1) # y-axis
par(op)

# 图1  泊松分布		
set.seed(123456)
barPois <- table(stats::rpois(1000, lambda = 5))
barplot(barPois, col = "lightblue",border = "white",main = "The Poisson Distribution")

# 图2 
data(NCI60,package ="ISLR") # 加载数据
dat <- as.data.frame(table(NCI60$labs))
order_dat <- dat[order(dat$Freq,decreasing = TRUE),]
num <- order_dat[,2]
names(num) <- order_dat[,1]
op <- par(mar=c(2,7,1,1))
plot(c(0,10),c(0,17),type="n",axes = FALSE,ann=FALSE)
xlabs <- seq(0,10,by=1) # 下面一行是关键
ylabs <- seq(0.7,length(num)+2.3,length.out=length(num))
ylabs_names <- names(num)
axis(1, labels = xlabs, at = xlabs, las = 1,col = "gray") # xaxis
axis(2, labels = ylabs_names, at = ylabs, las = 1,col="white")
barplot(num,add=TRUE,col="lightblue",axes = FALSE,
		axisnames=FALSE,border ="white",
		horiz=TRUE,xlab="numbers",ylab="")

# 图3
data(NCI60,package ="ISLR") # 加载数据		
myData <- sort(table(NCI60$labs))		
par(mar=c(5,2.5,1,1))	
barCenters <- barplot(myData,col = "lightblue",axes = FALSE,
			axisnames=FALSE,border ="white")				
text(x = barCenters, y = par("usr")[3]-.2, srt = 45,
     adj = 1, labels = names(myData), xpd = TRUE)
axis(2, labels = seq(0,9,by=1), at = seq(0,9,by=1),  las = 1 , col = "gray") 

# 图4 参考来自 https://cosx.org/2017/05/random-number-generation/
set.seed(1234)
n <- 2^24
x <- runif(n,0,1)
delta <- 0.01
len <- diff(c(0,which(x < delta),n+1))-1
ylim <- seq( 0, 1800, by = 200)
xlim <- seq( 0, 100, by = 10)
par(mar=c(2,2,.5,.5))
hist(len[len < 101], col="lightblue",
	 breaks = -1:100+0.5,
	 border = "white", axes = FALSE, ann = FALSE)
axis( 1, labels = xlim, at = xlim, las = 1) # x 轴
axis( 2, labels = ylim, at = ylim, las = 0) # y 轴
box(col="gray")

# 图5  点图  mtcars 
mtcars4 <- mtcars[mtcars$cyl == 4,]
mtcars6 <- mtcars[mtcars$cyl == 6,]
mtcars8 <- mtcars[mtcars$cyl == 8,]
myData <- rbind( mtcars4[order(mtcars4$mpg,decreasing = FALSE ), ],
		 mtcars6[order(mtcars6$mpg,decreasing = FALSE ), ],
		 mtcars8[order(mtcars8$mpg,decreasing = FALSE ), ] )

pdf(file = "dotplot.pdf",width=6,height=8)
par(mar =c(2,8,0,4) )
with(myData,{
	barCenters <- barplot(mpg,horiz = TRUE,axes = FALSE,xlim=c(0,35),
				col = rev(paste0("lightblue",seq(3)))[as.factor(cyl)],
				space =4.5, border = "white",width = .1)
	text(y = barCenters, x = par("usr")[3],
		 adj = 1, labels = rownames(myData), xpd = TRUE) # 纵轴标签
	points(y = barCenters, x = mpg, pch= 19, cex = 2.5,
			col = rev(paste0("lightblue",seq(3)))[as.factor(cyl)] )  # 加点
	text(y = barCenters, x = mpg, labels = mpg,cex=.5) # 加数字
	legend("topright", legend = c("8","6","4"),title = "cyl",
			col = c("lightblue1","lightblue2","lightblue3"),pch = 19,
			border = "white",box.col = "white", pt.cex = 2.5,cex = 1.5) # 加图例
	axis(1, labels = seq(0,35,by=5), at = seq(0,35,by=5),  las = 1 , col = "gray")	# 横轴 	
})
dev.off()

# 图6
packages <- readRDS(file = "packages.rds")
packages <- as.data.frame(packages,stringsAsFactors = FALSE)

maintainer <- gsub("<([^<>]*)>","",packages$Maintainer)
maintainer <- gsub("(^\\s*)|(\\s*$)|(\\\")|(\\\')|(')","",maintainer)

top_pkgs_dev <- sort(table(maintainer),decreasing = TRUE)
dat1 <- as.data.frame(top_pkgs_dev[top_pkgs_dev > 11])
dat1 <- dat1[dat1$maintainer != "ORPHANED",] 
pdf(file = "dotplot.pdf", width = 6, height = 8)
op <- par(mar=c(4,9,0,1))
plot(c(0,80),c(0,47),type= "n",axes = FALSE,xlab = "# of Packages",ylab ="")
barCenters <- barplot(dat1[,2],col = "lightblue",axes = FALSE,
			space = 4.5, width = .2,
			axisnames=FALSE,horiz=TRUE,border ="white", add = TRUE)	
points(y = barCenters, x = dat1[,2], pch= 19, 
	cex = 1.5, col = "lightblue4")  # 加点	
text(y = barCenters, x = dat1[,2], labels = dat1[,2],cex = .5,col = "white") # 加数字		
text(y = barCenters, x = par("usr")[3],
     adj = 1, labels = dat1[,1], xpd = TRUE)
axis(1, labels = seq(0,80,by = 10), at = seq(0,80, by=10),  las = 1 , col = "gray")
dev.off()

# 图7
data(NCI60,package ="ISLR") # 加载数据
dat <- as.data.frame(table(NCI60$labs))
order_dat <- dat[order(dat$Freq,decreasing = TRUE),]
num <- order_dat[,2]
names(num) <- order_dat[,1]
op <- par(mar=c(2,1,1,1))
plot(c(0,10),c(0,17),type="n",axes = FALSE,ann=FALSE)
xlabs <- seq(0,10,by=1)
axis(1, labels = xlabs, at = xlabs, las = 1) # x-axis
mycolors <- yarrr::piratepal(palette = "info2") 
barplot(num,add=TRUE,col=mycolors,axes = FALSE,
axisnames=FALSE,border ="white",
args.legend=list(border = "white",box.col="white"),
horiz=TRUE,xlab="numbers",ylab="",legend.text = TRUE)
box(col = "gray")
par(op)

# 图8 同图2

# 图9  Gambia儿童疟疾
library(sjPlot)
library(sjmisc)
library(ggplot2)

data(gambia,package="geoR")
dat1 <- subset(gambia,netuse==0) # 0 没有使用蚊帐
dat1$Index <- rep(0,dim(dat1)[1])
dat2 <- subset(gambia,netuse==1 & treated == 0)  # 1
dat2$Index <- rep(1,dim(dat2)[1])
dat3 <- subset(gambia,netuse==1 & treated == 1)  # 2 
dat3$Index <- rep(2,dim(dat3)[1])
mydata <- rbind(dat1,dat2,dat3)

sjp.setTheme(geom.outline.color = "white",
	     axis.linecolor = "white", base = theme_classic())

pdf(file="bed_net.pdf",width = 6,height = 5.25)
sjp.grpfrq( mydata$Index,mydata$pos,  geom.colors = c("lightblue","lightblue4"),
	    axis.titles="Childhood malaria in the Gambia",
	    axis.labels= c("net not use","net not treated","net treated"),
	    legend.title="RDT")
dev.off()



# 图10
barplot(t(VADeaths), col = rev(paste0("lightblue",seq(4))),
		border = "white", axes = FALSE, ylim = c(0, 241.4),
		legend.text = colnames(VADeaths),
		args.legend = list(x = "topleft",border = "white",
				   box.col = "white", pt.cex = 2) )		
axis(2, labels = seq(0,240,by=40), at = seq(0,240,by=40),  las = 1 , col = "gray")		
		
# 图11 这是gnuplot软件的绘图代码 
# 图11 12 14 15 代码来自 http://gnuplot.sourceforge.net/demo/histograms2.html 和 http://gnuplot.sourceforge.net/demo/histograms.html
# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'histograms.4.png'
set bar 1.000000 front
set boxwidth 0.75 absolute
set style fill   solid 1.00 border lt -1
set style circle radius graph 0.02, first 0.00000, 0.00000 
set style ellipse size graph 0.05, 0.03, first 0.00000 angle 0 units xy
set key outside right top vertical Left reverse noenhanced autotitle columnhead nobox
set key invert samplen 4 spacing 1 width 0 height 0 
set style histogram rowstacked title textcolor lt -1
set style textbox transparent margins  1.0,  1.0 border
unset logscale
set datafile missing '-'
set style data histograms
set xtics border in scale 0,0 nomirror rotate by -45  autojustify
set xtics  norangelimit 
set xtics   ()
unset paxis 1 tics
unset paxis 2 tics
unset paxis 3 tics
unset paxis 4 tics
unset paxis 5 tics
unset paxis 6 tics
unset paxis 7 tics
set title "US immigration from Europe by decade\nPlot as stacked histogram" 
set yrange [ 0.00000 : 7.00000e+06 ] noreverse nowriteback
set paxis 1 range [ * : * ] noreverse nowriteback
set paxis 2 range [ * : * ] noreverse nowriteback
set paxis 3 range [ * : * ] noreverse nowriteback
set paxis 4 range [ * : * ] noreverse nowriteback
set paxis 5 range [ * : * ] noreverse nowriteback
set paxis 6 range [ * : * ] noreverse nowriteback
set paxis 7 range [ * : * ] noreverse nowriteback
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.05, 0.6, 0 front  noinvert bdefault
x = 0.0
i = 23
## Last datafile plotted: "immigration.dat"
plot 'immigration.dat' using 2:xtic(1), for [i=3:22] '' using i

# 图12 这是gnuplot软件的绘图代码
# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'histograms.6.png'
set bar 1.000000 front
set border 3 front lt black linewidth 1.000 dashtype solid
set boxwidth 0.75 absolute
set style fill   solid 1.00 border lt -1
set style circle radius graph 0.02, first 0.00000, 0.00000 
set style ellipse size graph 0.05, 0.03, first 0.00000 angle 0 units xy
set grid nopolar
set grid noxtics nomxtics ytics nomytics noztics nomztics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault   lt 0 linewidth 0.500,  lt 0 linewidth 0.500
set key outside right top vertical Left reverse noenhanced autotitle columnhead box lt black linewidth 1.000 dashtype solid
set style histogram columnstacked title textcolor lt -1
set style textbox transparent margins  1.0,  1.0 border
unset logscale
set datafile missing '-'
set style data histograms
set xtics border in scale 1,0.5 nomirror norotate  autojustify
set xtics  norangelimit 
set xtics   ()
set ytics border in scale 0,0 mirror norotate  autojustify
set ztics border in scale 0,0 nomirror norotate  autojustify
set cbtics border in scale 0,0 mirror norotate  autojustify
set rtics axis in scale 0,0 nomirror norotate  autojustify
set paxis 1 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 1 tics  rangelimit autofreq 
set paxis 2 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 2 tics  rangelimit autofreq 
set paxis 3 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 3 tics  rangelimit autofreq 
set paxis 4 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 4 tics  rangelimit autofreq 
set paxis 5 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 5 tics  rangelimit autofreq 
set paxis 6 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 6 tics  rangelimit autofreq 
set paxis 7 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 7 tics  rangelimit autofreq 
set title "Immigration from Northern Europe\n(columstacked histogram)" 
set xlabel "Country of Origin" 
set ylabel "Immigration by decade" 
set yrange [ 0.00000 : * ] noreverse nowriteback
set paxis 1 range [ * : * ] noreverse nowriteback
set paxis 2 range [ * : * ] noreverse nowriteback
set paxis 3 range [ * : * ] noreverse nowriteback
set paxis 4 range [ * : * ] noreverse nowriteback
set paxis 5 range [ * : * ] noreverse nowriteback
set paxis 6 range [ * : * ] noreverse nowriteback
set paxis 7 range [ * : * ] noreverse nowriteback
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.05, 0.6, 0 front  noinvert bdefault
x = 0.0
i = 24
## Last datafile plotted: "immigration.dat"
plot 'immigration.dat' using 6 ti col, '' using 12 ti col,      '' using 13 ti col, '' using 14:key(1) ti col


# 图13 复合条形图
par(mfrow = c(1,6),mar = c(4.5,2.5,0,1.5))
layout(mat = matrix(seq(6), ncol = 6, byrow = TRUE))
for(i in seq(6)){
	plot(c(0,2),c(0,850), axes = FALSE, type = "n", xlab = "",ylab = "")
	barplot(UCBAdmissions[,,i], col = c("lightblue3","lightblue1"),axes = FALSE,
		border = "white", xlab = paste0("Dept = ", LETTERS[i]), add = TRUE)
   if(i == 1 ){
	axis(2, labels = seq(0,800, by = 100), at = seq(0,800, by = 100), 
		las = 0 , col = "gray")
	}		
   if( i ==6 ){
	legend("topleft", legend = c("Rejected","Admitted"),
		col = c("lightblue1","lightblue3"),pch = 15,
		border = "white",box.col = "white", pt.cex = 4,cex = 1.5)
	}	
}

# 图14 这是 gnuplot软件的绘图代码 immigration.dat是软件自带的数据集
# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'histograms.7.png'
set bar 1.000000 front
set border 3 front lt black linewidth 1.000 dashtype solid
set boxwidth 0.95 absolute
set style fill   solid 1.00 noborder
set style circle radius graph 0.02, first 0.00000, 0.00000 
set style ellipse size graph 0.05, 0.03, first 0.00000 angle 0 units xy
set grid nopolar
set grid noxtics nomxtics ytics nomytics noztics nomztics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault   lt 0 linewidth 0.500,  lt 0 linewidth 0.500
set key bmargin center horizontal Left reverse noenhanced autotitle columnhead nobox
set style histogram clustered gap 1 title textcolor lt -1 offset character 2, 0.25, 0
set style textbox transparent margins  1.0,  1.0 border
unset logscale
set datafile missing '-'
set style data histograms
set xtics border in scale 0,0 nomirror rotate by -45  autojustify
set xtics  norangelimit  font ",8"
set xtics   ()
set ytics border in scale 0,0 mirror norotate  autojustify
set ytics  norangelimit autofreq  font ",8"
set ztics border in scale 0,0 nomirror norotate  autojustify
set cbtics border in scale 0,0 mirror norotate  autojustify
set rtics axis in scale 0,0 nomirror norotate  autojustify
set paxis 1 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 1 tics  rangelimit autofreq 
set paxis 2 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 2 tics  rangelimit autofreq 
set paxis 3 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 3 tics  rangelimit autofreq 
set paxis 4 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 4 tics  rangelimit autofreq 
set paxis 5 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 5 tics  rangelimit autofreq 
set paxis 6 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 6 tics  rangelimit autofreq 
set paxis 7 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 7 tics  rangelimit autofreq 
set title "Immigration from different regions" 
set xlabel " " 
set xlabel  offset character 0, -2, 0 font "" textcolor lt -1 norotate
set ylabel "Immigration by decade" 
set paxis 1 range [ * : * ] noreverse nowriteback
set paxis 2 range [ * : * ] noreverse nowriteback
set paxis 3 range [ * : * ] noreverse nowriteback
set paxis 4 range [ * : * ] noreverse nowriteback
set paxis 5 range [ * : * ] noreverse nowriteback
set paxis 6 range [ * : * ] noreverse nowriteback
set paxis 7 range [ * : * ] noreverse nowriteback
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.05, 0.6, 0 front  noinvert bdefault
x = 0.0
i = 24
## Last datafile plotted: "immigration.dat"
plot newhistogram "Northern Europe", 'immigration.dat' using "Sweden":xtic(1) t col, '' u "Denmark" t col, '' u "Norway" t col, newhistogram "Southern Europe", '' u "Greece":xtic(1) t col, '' u "Romania" t col, '' u "Yugoslavia" t col, newhistogram "British Isles", '' u "Ireland":xtic(1) t col, '' u "United_Kingdom" t col

# 图15 这是 gnuplot软件的绘图代码
# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'histograms.8.png'
set bar 1.000000 front
set border 3 front lt black linewidth 1.000 dashtype solid
set boxwidth 0.8 absolute
set style fill   solid 1.00 noborder
set style circle radius graph 0.02, first 0.00000, 0.00000 
set style ellipse size graph 0.05, 0.03, first 0.00000 angle 0 units xy
set grid nopolar
set grid noxtics nomxtics ytics nomytics noztics nomztics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault   lt 0 linewidth 0.500,  lt 0 linewidth 0.500
set key bmargin center horizontal Left reverse noenhanced autotitle columnhead nobox
set style histogram rowstacked title textcolor lt -1 offset character 2, 0.25, 0
set style textbox transparent margins  1.0,  1.0 border
unset logscale
set datafile missing '-'
set style data histograms
set xtics border in scale 0,0 nomirror rotate by -45  autojustify
set xtics  norangelimit  font ",8"
set xtics   ()
set ytics border in scale 0,0 mirror norotate  autojustify
set ytics  norangelimit autofreq  font ",8"
set ztics border in scale 0,0 nomirror norotate  autojustify
set cbtics border in scale 0,0 mirror norotate  autojustify
set rtics axis in scale 0,0 nomirror norotate  autojustify
set paxis 1 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 1 tics  rangelimit autofreq 
set paxis 2 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 2 tics  rangelimit autofreq 
set paxis 3 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 3 tics  rangelimit autofreq 
set paxis 4 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 4 tics  rangelimit autofreq 
set paxis 5 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 5 tics  rangelimit autofreq 
set paxis 6 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 6 tics  rangelimit autofreq 
set paxis 7 tics border in scale 0,0 nomirror norotate  autojustify
set paxis 7 tics  rangelimit autofreq 
set title "Immigration from different regions" 
set xlabel " " 
set xlabel  offset character 0, -2, 0 font "" textcolor lt -1 norotate
set ylabel "Immigration by decade" 
set yrange [ 0.00000 : 900000. ] noreverse nowriteback
set paxis 1 range [ * : * ] noreverse nowriteback
set paxis 2 range [ * : * ] noreverse nowriteback
set paxis 3 range [ * : * ] noreverse nowriteback
set paxis 4 range [ * : * ] noreverse nowriteback
set paxis 5 range [ * : * ] noreverse nowriteback
set paxis 6 range [ * : * ] noreverse nowriteback
set paxis 7 range [ * : * ] noreverse nowriteback
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.05, 0.6, 0 front  noinvert bdefault
x = 0.0
i = 24
## Last datafile plotted: "immigration.dat"
plot newhistogram "Northern Europe", 'immigration.dat' using "Sweden":xtic(1) t col, '' u "Denmark" t col, '' u "Norway" t col, newhistogram "Southern Europe", '' u "Greece":xtic(1) t col, '' u "Romania" t col, '' u "Yugoslavia" t col, newhistogram "British Isles", '' u "Ireland":xtic(1) t col, '' u "United_Kingdom" t col

# 图16：复合条形图的另类
pdf(file = "UCBAdmissions2.pdf",width=8)  # 马赛克图 强调 各系之间的对比
op <- par(mar = c(2,2,2.5,0))
# mosaicplot(UCBAdmissions, col = "lightblue", border = "white" )
mosaicplot(~Dept + Admit + Gender,data = UCBAdmissions, col = "lightblue", border = "white" )
par(op)
dev.off()

# 图17：复合堆积条形图	
pdf(file = "stackplot.pdf")
layout(mat = matrix(seq(2), ncol = 2, byrow = TRUE))	
plot(c(0,2),c(0,120), axes = FALSE, type = "n", xlab = "",ylab = "")		
barplot(HairEyeColor[,,2],width = .5,col = rev(paste0("lightblue",seq(4))),
        border = "white",main = "Female",xlab = "Eye",add = TRUE)
plot(c(0,2),c(0,120), axes = FALSE, type = "n", xlab = "",ylab = "")	
barplot(HairEyeColor[,,1],width = .5,col = rev(paste0("lightblue",seq(4))), axes = FALSE,
        border = "white",main = "Male",xlab = "Eye",add = TRUE)			
legend(x= 1.2,y=120, legend = c("Black", "Brown", "Red", "Blond"),title = "Hair",
       col = rev(paste0("lightblue",seq(4))),pch = 15,
       border = "white",box.col = "white", pt.cex = 3,cex = 1)	
dev.off()

# 图18：马赛克图：用马赛克图代替复杂复合条形图
pdf(file = "stackmosaicplot.pdf")
op <- par(mar = c(2,2,2.5,0))
mosaicplot(HairEyeColor, col = "indianred", border = "white") 
# 数据为多维数组"lightgray" "lightblue" indianred
par(op)
dev.off()

# 图19 国际航班乘客示意图	

AirPassengers2 <- matrix(AirPassengers, ncol = 12, byrow = TRUE,
            dimnames = list( 1948 + seq(12),
                    c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) )	
					
barplot(colMeans(AirPassengers2),col = "lightblue",
		border = "white",main = "Monthly Average Airline Passenger Numbers 1949-1960")
		
# 图20 同图1

# 图21
library(ggplot2)
data <- data.frame(type = c("线路","电动机","变压器","发电机"), 
                   price = c(25, 30, 45, 60), num = c(50, 35, 20, 10))
ggplot(data,aes(reorder(type, price), price, fill = num[order(num)]))+
  geom_col()+
  xlab("类型")+
  ylab("价格")+
  scale_fill_continuous(name="数量")

# 图22
dat2 <- as.data.frame(sort(table(table(maintainer)),decreasing = TRUE))
dat2 <- rbind(head(dat2,10),data.frame(Var1 = factor(">10"),Freq= 53))
dat2$Var1 <- as.factor(dat2$Var1)
dat2$Freq <- as.numeric(dat2$Freq)	   
	    
library(ggplot2)
library(scales)  
library(ggthemes) 
ggplot(dat2,aes(x = reorder(Var1,Freq),y = Freq)) + 
  geom_bar(stat="identity",width = 0.5,fill = "lightblue") +
  annotation_logticks(sides = "l") + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x),
		labels = trans_format("log10",math_format(10^.x))) +  
  geom_text( aes(label = Freq),vjust = -0.2,colour = "black") +			
  theme_few() +
  theme(panel.grid.major.x = element_blank())+
  xlab(" # of Packages ") +
  ylab(" # of Developers ") 	   
	   

# 图23 代码来自 https://github.com/cpwardell/3dbarplot
library(rgl)
## Draws a single "column" or "stack".
## X and Y coordinates determine the area of the column
## The Z coordinate determines the height of the column
## We include "lit=FALSE" arguments to remove the nasty shiny surfaces caused by lighting
stackplot.3d<-function(x,y,z,alpha=1,topcol="#078E53",sidecol="#aaaaaa"){
  
  ## These lines allow the active rgl device to be updated with multiple changes
  ## This is necessary to draw the sides and ends of the column separately  
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))
  
  ## Determine the coordinates of each surface of the column and its edges
  x1=c(rep(c(x[1],x[2],x[2],x[1]),3),rep(x[1],4),rep(x[2],4))
  z1=c(rep(0,4),rep(c(0,0,z,z),4))
  y1=c(y[1],y[1],y[2],y[2],rep(y[1],4),rep(y[2],4),rep(c(y[1],y[2],y[2],y[1]),2))
  x2=c(rep(c(x[1],x[1],x[2],x[2]),2),rep(c(x[1],x[2],rep(x[1],3),rep(x[2],3)),2))
  z2=c(rep(c(0,z),4),rep(0,8),rep(z,8) )
  y2=c(rep(y[1],4),rep(y[2],4),rep(c(rep(y[1],3),rep(y[2],3),y[1],y[2]),2) )
  
  ## These lines create the sides of the column and its coloured top surface
  rgl.quads(x1,z1,y1,col=rep(sidecol,each=4),alpha=alpha,lit=FALSE)
  rgl.quads(c(x[1],x[2],x[2],x[1]),rep(z,4),c(y[1],y[1],y[2],y[2]),
            col=rep(topcol,each=4),alpha=1,lit=FALSE) 
  ## This line adds black edges to the column
  rgl.lines(x2,z2,y2,col="#000000",lit=FALSE)
}

## Calls stackplot.3d repeatedly to create a barplot
## z is the heights of the columns and must be an appropriately named vector
context3d<-function(z,alpha=1,scalexy=10,scalez=1,gap=0.2){
  ## These lines allow the active rgl device to be updated with multiple changes
  ## This is necessary to add each column sequentially
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))

  ## Recreate Broad order
  types=c("C.G.G.C","T.A.A.T","C.A.G.T","T.G.A.C","C.T.G.A","T.C.A.G")
  contexts=c("TxT","CxT","AxT","GxT","TxC","CxC","AxC","GxC",
             "TxA","CxA","AxA","GxA","TxG","CxG","AxG","GxG")
  typeorder=c()
  for(type in types){
    typeorder=c(typeorder,paste(type,contexts,sep="_"))
  }
  z=z[typeorder]
  
  ## Reorder data into 6 regions
  set1=c(1:4,17:20,5:8,21:24,9:12,25:28,13:16,29:32)
  set2=set1+32
  set3=set1+64
  neworder=c(set1,set2,set3)

  ## Define dimensions of the plot 
  dimensions=c(12,8)
  
  ## Scale column area and the gap between columns 
  y=seq(1,dimensions[1])*scalexy
  x=seq(1,dimensions[2])*scalexy
  gap=gap*scalexy
  
  ## Scale z coordinate
  z=z*scalez
  
  ## Set up colour palette
  broadcolors=c("#805D3F","#72549A","#5EAFB2","#3F4F9D","#F2EC3C","#74B655")
  colors=as.vector(sapply(broadcolors,rep,16))
  
  ## Plot each of the columns
  for(i in 1:dimensions[1]){
    for(j in 1:dimensions[2]){
      it=(i-1)*dimensions[2]+j # Variable to work out which column to plot; counts from 1:96
      stackplot.3d(c(gap+x[j],x[j]+scalexy),
                   c(-gap-y[i],-y[i]-scalexy),
                   z[neworder[it]],
                   alpha=alpha,
                   topcol=colors[neworder[it]],
                   sidecol=colors[neworder[it]])
    }
  }
  ## Set the viewpoint and add axes and labels
  rgl.viewpoint(theta=50,phi=40,fov=0)
  axes3d("y-+",labels=TRUE)
}

## Read in example data and cast to an appropriate vector
rawdata=read.table("snvspermegabase.txt",header=TRUE)
counts=as.numeric(rawdata)
names(counts)=colnames(rawdata)
## Example plots
context3d(counts,alpha=0.4)
########################################

# 图24 每月航班人数的变化						
x <- 1948 + seq(12)
y <- seq(from = 1 ,to = 12, length.out = 12)
z <- matrix(AirPassengers, ncol = 12, byrow = TRUE,
            dimnames = list( 1948 + seq(12),
                    c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) )
library(colormap)
library(plot3D)
colorBar <- colormap_pal(colormap=colormaps$viridis, reverse = TRUE,alpha = 1)(10)
pdf(file="AirlinePassenger.pdf")
par(mar =c(2,0,2,0) )
hist3D(x, y, z, xlab = "Year", ylab = "Month", zlab = "# of Airline Passengers",
	main = "Monthly Airline Passenger Numbers 1949-1960",
	col = colorBar, colkey = FALSE,theta=30, phi=20,r=50,d=0.1,
	expand = 0.5,ltheta=-55, lphi=180, lighting = TRUE,
	ticktype = "detailed",space = .3, bty = "b2",facets = FALSE )
dev.off()

# 图25  代码来自 https://cn.mathworks.com/matlabcentral/profile/authors/4199206-johannes-schmitz
function [] = plot_bar3_colorheights( x, y, Z )
%PLOT_BAR3_COLORHEIGHTS bar3 plot with color coded heights
% x, y : vector of centers of bins
%    Z : matrix of bin heights

b = bar3(y,Z,1)

for k = 1:length(b)
    zdata = b(k).ZData;
    b(k).CData = zdata;
    b(k).FaceColor = 'interp';
end

axis tight

Xdat=get(b,'XData');
for ii=1:length(Xdat)
    Xdat{ii}=(Xdat{ii}-0.5)*(x(2)-x(1));
    set(b(ii),'XData',Xdat{ii});
end

end
 % 上面这段代码打包成函数 plot_bar3_colorheights		
plot_bar3_colorheights(1:1:12,1900:10:2000,temperatures)		
		
# 图26  Matlab 数据和代码来自 https://cn.mathworks.com/examples/matlab/community/19570-bar_plot_3d?s_tid=srchtitle
% Load monthly temperature data
load MonthlyTemps temperatures months years
% Create the 3D bar chart
figure
bar3(temperatures)
axis([0 13 0 12 0 80])

% Add title and axis labels
title('Boston Monthly Temperatures 1900-2000')
xlabel('Month')
ylabel('Year')
zlabel('Temperature')

% Change the x and y axis tick labels
set(gca, 'XTickLabel', months)
set(gca, 'YTickLabel', years)

# 图27和28：三维柱形图转平面格点图（又称棋盘图）
dat <- as.data.frame( cbind(rep( 1948 + seq(12), each = 12), rep(seq(12),12), AirPassengers) )  
colnames(dat) <- c("year","month","passengers")
library(ggplot2)

pdf(file="AirlinePassenger.pdf")
ggplot(data = dat, aes(as.factor(year), as.factor(month))) + 
  geom_point(pch = 15,size = 8 ,aes(colour = passengers )) +
  scale_colour_distiller(palette = "Spectral") +
  labs(x = "Year",y = "Month",colour = "Passengers") +
  theme_bw()

library(colormap)	
ggplot(data = dat, aes(as.factor(year), as.factor(month))) + 
  geom_point(pch = 15,size = 8 ,aes(color = passengers )) +
  scale_color_colormap(colormap = colormaps$viridis) +
  labs(x = "Year",y = "Month",colour = "Passengers") +
  theme_bw()	
dev.off()

# 附：
# 图10 的点图画法
dotchart(VADeaths, main = "Death Rates in Virginia - 1940",bg="lightblue",
			pch =21,color = "lightblue",lcolor = "gray",pt.cex=2)

# 图5 
library(ggpubr)
data("mtcars")
dfm <- mtcars
# Convert the cyl variable to a factor
dfm$cyl <- as.factor(dfm$cyl)
# Add the name colums
dfm$name <- rownames(dfm)

ggdotchart(dfm, x = "name", y = "mpg",
           xlab = "32 automobiles (1973–74 models)",
           color = "cyl",                                # Color by groups
           palette = c("lightblue3", "lightblue2", "lightblue1"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "cyl",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(dfm$mpg),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
           )						
						

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

# 图4



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
dat1 <- as.data.frame(top_pkgs_dev[top_pkgs_dev > 10])
dat1 <- dat1[dat1$maintainer != "ORPHANED",] # top 53 开发者 pkg >10 

library(ggplot2)
ggplot(dat1,aes(x = Freq, y = maintainer )) +  
  geom_segment(aes(xend = 0, yend = maintainer ),colour = "grey50" )+
  geom_point(size=2,colour = "red") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())+
  xlab(" # of Packages ") +
  ylab(" Developer ") +
  labs(title= " Top 53 developers ",
       caption = "Data source: https://mran.microsoft.com/snapshot/2017-07-15/web/packages/packages.rds") 

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

sjp.setTheme(theme.font="Times",geom.outline.color = "white",
	     axis.linecolor = "white", base = theme_classic())

pdf(file="bed_net.pdf",width = 6,height = 5.25)
sjp.grpfrq(	mydata$Index,mydata$pos,  geom.colors = "Set1",
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
		
# 图11 图12	

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

# 图14 图15

# 图16 国际航班乘客示意图	

AirPassengers2 <- matrix(AirPassengers, ncol = 12, byrow = TRUE,
            dimnames = list( 1948 + seq(12),
                    c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) )	
					
barplot(colMeans(AirPassengers2),col = "lightblue",
		border = "white",main = "Monthly Average Airline Passenger Numbers 1949-1960")
		
# 图17 同图1

# 图18

# 图19
dat2 <- as.data.frame(sort(table(table(maintainer)),decreasing = TRUE))
dat2 <- rbind(head(dat2,10),data.frame(Var1 = factor(">10"),Freq= 53))
dat2$Var1 <- as.factor(dat2$Var1)
dat2$Freq <- as.numeric(dat2$Freq)	   
	      
library(scales)  
ggplot(dat2,aes(x = reorder(Var1,Freq),y = Freq)) + 
  geom_bar(stat="identity",width = 0.5) +
  annotation_logticks(sides = "l") + 
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x),
		labels = trans_format("log10",math_format(10^.x))) +  
  geom_text( aes(label = Freq),vjust = -0.2,colour = "black") +			
  theme_gdocs() +
  theme(panel.grid.major.x = element_blank())+
  xlab(" # of Packages ") +
  ylab(" # of Developers ") 	   
	   

# 图20

# 图21 每月航班人数的变化						
x <- 1948 + seq(12)
y <- seq(from = 12 ,to = 1, length.out = 12)
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

# 图22
# 图23






# 图10 的点图画法
dotchart(VADeaths, main = "Death Rates in Virginia - 1940",bg="lightblue",
			pch =21,color = "lightblue",lcolor = "gray",pt.cex=2)
									
# 复合堆积条形图的反例	
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

# 用马赛克图代替复杂复合条形图  矩阵树图
pdf(file = "stackmosaicplot.pdf")
op <- par(mar = c(2,2,2.5,0))
mosaicplot(HairEyeColor, col = "indianred", border = "white") 
# 数据为多维数组"lightgray" "lightblue" indianred
par(op)
dev.off()

# 复合条形图的另类
pdf(file = "UCBAdmissions2.pdf",width=8)  # 马赛克图 强调 各系之间的对比
op <- par(mar = c(2,2,2.5,0))
# mosaicplot(UCBAdmissions, col = "lightblue", border = "white" )
mosaicplot(~Dept + Admit + Gender,data = UCBAdmissions, col = "lightblue", border = "white" )
par(op)
dev.off()
			
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
						









		

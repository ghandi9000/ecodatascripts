# Test slope
################################################
############	Coords, Radians	############
################################################
x <- pp$BQUDX
newx <- c(-4.5:-.5,.5:4.5)
for(i in 1:10) { x[x==i] <- newx[i] }
y <- pp$BQUDY
newy <- c(-4.5:-.5,0.5:4.5)
for(i in 1:10) { y[y==i] <- newy[i] }
pp$XCOORD <- x
pp$YCOORD <- y
# radians
pp$SLOPERAD <- pp$SLOPE8687*2*pi/360
pp$ASPRAD <- 2*pi - (pp$ASP*2*pi/360)
##########################################
############	Functions	############
##########################################
polar.angle <- function(x.coord,y.coord) {
	x <- abs(5.5-x.coord)
	y <- abs(5.5-y.coord)
	r <- sqrt(x^2+y^2)
	if(x.coord>=5.5 & y.coord<=5.5) theta <- pi/4 + acos(y/r) # Quad I
	if(x.coord<5.5 & y.coord<= 5.5) {	# Quad II
		if(abs(y/x)>=1) { theta <- (pi/4) - acos(y/r) }
		if(abs(y/x)<1) { theta <- 2*pi- (acos(y/r)-pi/4) }
	}
	if(x.coord<5.5 & y.coord>5.5) theta <- 5*pi/4 + acos(y/r)	# Quad III
	if(x.coord>=5.5 & y.coord>5.5) theta <- 3*pi/4 + asin(y/r)	# Quad IV
	theta
}
# rotates points cartesian coords counter-clockwise with respect to the origin by scalar theta
rotate.loci <- function(x,y,theta) {
	new.x <- x*cos(theta)+y*sin(theta)
	new.y <- -x*sin(theta) + y*cos(theta)
	c(new.x,new.y)
}

##########################################
############	Get Z vals	############
##########################################
p4 <- subset(pp, PLOT == 4 & !is.na(pp$XCOORD))
xp <- apply(p4[,c("XCOORD","YCOORD","ASPRAD")],1,function(x) { rotate.loci(x[["XCOORD"]],x[["YCOORD"]],mean(p4$ASPRAD))[1] })
yp <- apply(p4[,c("XCOORD","YCOORD","ASPRAD")],1,function(x) { rotate.loci(x[["XCOORD"]],x[["YCOORD"]],mean(p4$ASPRAD))[2] })
using <- which( xp > -6 )
plot(xp[using],yp[using])
abline(h=0, v=0)

x <- rep(-4.5:4.5,10)
y <- sort(rep(-4.5:4.5,10))
coords <- data.frame(x,y)
yp <- rotate.loci(coords["x"],coords["y"],mean(pp[pp$PLOT==4,]$ASPRAD,na.rm=T))[[2]]
yp2 <- rotate.loci(coords["x"],coords["y"],mean(pp[pp$PLOT==8,]$ASPRAD,na.rm=T))[[2]]
yp3 <- rotate.loci(coords["x"],coords["y"],mean(pp[pp$PLOT==27,]$ASPRAD,na.rm=T))[[2]]

test.slope <- wireframe(yp*tan(mean(pp[pp$PLOT==4,]$SLOPERAD,na.rm=T))+
	yp2*tan(mean(pp[pp$PLOT==8,]$SLOPERAD,na.rm=T)) +
	yp3*tan(mean(pp[pp$PLOT==27,]$SLOPERAD,na.rm=T)) ~ coord$x*coord$y, outer=T, layout=c(3,1),zlab = "altitude")

##########################################
############	Graphs	############
##########################################
x <- rep(1:10,10)
y <- sort(rep(1:10,10))
coords <- data.frame(x,y)
alt <- function(x,y,slope,aspect) {
	x <- 2*(5.5-x)
	y <- 2*(5.5-y)
	a <- sqrt(x^2+y^2)
	asp <- aspect*2*pi/360
	slo <- slope*2*pi/360
	z <- sqrt(x^2+y^2)*cos(acos(y/a)+(pi/4)-asp)*tan(slo)
	z
}

alt(1,1,30,45)
z.30 <- apply(coords, 1, function(x) { alt(x["x"],x["y"],slope=30,aspect=45) })
z.20 <- apply(coords, 1, function(x) { alt(x["x"],x["y"],slope=20,aspect=45) })
z.40 <- apply(coords, 1, function(x) { alt(x["x"],x["y"],slope=40,aspect=45) })

zasp.10<- apply(coords, 1, function(x) { alt(x["x"],x["y"],slope=20,aspect=10) })
zasp.169<- apply(coords, 1, function(x) { alt(x["x"],x["y"],slope=20,aspect=169) })
zasp.330<- apply(coords, 1, function(x) { alt(x["x"],x["y"],slope=20,aspect=330) })

coord <- cbind(coords, z.30,z.20,z.40,zasp.10,zasp.169, zasp.330)

library(lattice)
test.slope <- wireframe(coord$z.20+coord$z.30+coord$z.40~ coord$x*coord$y, zlab = "", layout =c(3,1), outer=T,
	main="Aspect=45, Slope = 20-40")
test.asp <- wireframe(zasp.10+zasp.169+zasp.330~ coord$x*coord$y, zlab = "", layout =c(3,1), outer=T,
	main="Slope=20, Aspect Varying")










## Functions used in bole volume and height prediction

##################################################################
######						                                ######
######		Formulas for bole volume calculation			######
######						                                ######
##################################################################
### kozak taper equation, returns diameter, d, at specified height, h
### funny volumes below 0.9 DBH, so small trees excluded
kozak2004 <- function(D, H, species, h) {
	if(species == "ABBA") {
		a0 = 0.911
		a1 = 1.026
		a2 = -0.005
		b1 = 0.368
		b2 = -0.645
		b3 = 0.502
		b4 = 1.780
		b5 = 0.096
		b6 = -0.487
	} else if(species == "PIRU") {
		a0 = 0.940
		a1 = 0.998
		a2 = 0.010
		b1 = 0.508
		b2 = -0.636
		b3 = 0.355
		b4 = 1.687
		b5 = 0.078
		b6 = -0.242
	} else { return ( NA ) }
	z <- h/H
	p <- 1.3/H
	Q <- 1-z^(1/3)
	X <- (1-(h/H)^(1/3))/(1-p^(1/3))
	d <- a0*(D^a1)*(H^a2)*X^(b1*(z^4)+b2*(1/exp(D/H))+b3*(X^0.1)+b4*(1/D)+b5*(H^Q)+b6*X)
	return( d );
}

### Smalian formula, D1 and D2 are diameters at each end of log, L is length
### returns volume in cubic m
smalian <- function(D1, D2, L) {
	D1 <- 0.00007854*(D1^2)
	D2 <- 0.00007854*(D2^2)
	volume = ((D1+D2)/2)*L
	return( volume )
}

### Honer eq 1965
honer <- function(D, H, species) {
	D <- 0.393700787*D
	H <- 3.2808399*H
	if(species == "ABBA") {
		a = 2.139
		b = 301.634
	}
	if(species == "PIRU") {
		a = 0.691
		b = 363.676
	}
	V = 0.0283168466*D^2/(a+(b/H))
	return( V )
}

### this version of the honer equation takes dbh in cm and height in m
### thus returning cubic meter
honer2 <- function(dbh, height, species) {
	if (species == "BECO" | species == "BEPA") {
        a0 = 2.222; a1 = 91.554; a2 = 0.0043222
	} else if (species == "BEAL") {
        a0 = 1.449; a1 = 105.081; a2 = 0.004320
    } else if (species == "ACSA" | species == "ACSP" | species == "ACPE") {
		a0 =1.046; a1 = 117.035; a2 = 0.004334
	} else if (species == "FAGR") {
		dbh <- dbh/(1-0.04365*0.145)
		a0=0.959; a1 = 102.056; a2 = 0.004334
    } else if (species == "PRPE" | species == "SOAM") {
        a0 = 0.033; a1 = 119.889; a2 = 0.004334
	} else { return ( NA ) }
    V = (a2*dbh^2)/(a0+(a1/height))
	return( V )
}

###################### Bole Volume Formula For Kozak BV ##########################
bolevol <- function(dbh, ht, species) {
    if (species != "ABBA" && species != "PIRU") return( NA )
	increment <- ht/10
	heights <- seq(from=0,to=ht, by = increment)
	diams <- kozak2004(dbh,ht,species=species,heights)
	volume = 0
	for(j in 1:10) { volume=volume+smalian(diams[[j]],diams[[j+1]],increment) }
	return( volume )
}

###################### Clark BV Equation #########################################
### Requires trees to be 17.3 feet tall and 5 in diameter
### input units are dbh in cm, height in m, output is in m^3

clark <- function(dbh, height, species) {
	##### Species Parameters ########################
	if(species=="PRPE" | species=="SOAM") {
        a1 = 0.92487; b1 = -0.89867;  r = 37.12714; c = 0.48776; e = 1.50579;
		p = 6.18866; b = 1.64261; a = 0.55071
    } else if(species=="ACPE" | species=="ACSA" | species=="ACSP") {
        a1 = 0.93991; b1 = -1.62226; r = 22.00135; c = 0.45472;
		e = 166.1; p = 7.31546; b = 1.17064; a = 0.27213
    } else if(species=="FAGR") {
        a1 = 0.91141; b1 = -0.6673; r = 44.36826; c = 1.22158; e = 79.44636;
		p = 6.36236; b = 1.11382; a = 0.14312
    } else if(species=="BEAL" | species=="BECO" | species=="BEPA" | species=="BESPP") {
        a1 = 0.85516; b1 = -0.00134; r = 49.41385; c = 1.01241;
		e = -91.82769; p = 11.23179; b = 1.19704; a = 0.23928
    } else { return ( NA ) }
    ##	if(species=="ABBA" | species=="PIRU") { a1 = 0.92487; b1 = -0.89867;  r = 31.66250; c = 0.57402; e = 110.96;
    ##		p = 8.573; b = 2.36238; a = 0.68464 } # using the coefficients for loblolly pine to get comparisons for ABBA and PIRU
    ##if(species=="SOAM") { a1 = 0.91531; b1 = -0.96788; r = 0.64600; c = 0.49680; e = 127.87;
    ##	p = 7.52915; b = 1.49287; a = 0.47222 }

	### Basic Symbols ###############################
	### V <- stem volumen between L and U in cubic ft.
	### L <- lower height of interest
	### U <- upper height of interest
	### D <- diameter in inches at 4.5 ft
	### H <- total ht of tree in FT
	### F <- diameter at 17.3 ft above ground in inches, DOB17 = D(a+b(17.3/H)^2) where a,b are sp. coefs
	D <- dbh*0.393700787 ### convert dbh in cm to dbh in in.
	L <- 0
	U <- height*3.2808399 ### convert height in meters to height in feet
	H <- U
	F <- D*(a1+b1*(17.3/H)^2)
	### Combined Variables #########################
	L1 <- max(L,0)
	U1 <- min(U,4.5)
	L2 <- max(L,4.5)
	U2 <- min(U,17.3)
	L3 <- max(L,17.3)
	U3 <- min(U,H)
	G <- (1-4.5/H)^r
	W <- (c + e/D^3)/(1-G)
	X <- (1-4.5/H)^p
	Y <- (1-17.3/H)^p
	Z <- (D^2-F^2)/(X-Y)
	T <- (D^2-Z*X)

	### Indicator Variables ########################
	I1 <- if(L < 4.5) I1 <- 1 else(I1 <- 0)
	I2 <- if(L < 17.3) I2 <- 1 else(I2 <- 0)
	I3 <- if(U > 4.5) I3 <- 1 else(I3 <- 0)
	I4 <- if(U > 17.3) I4 <- 1 else(I4 <- 0)
	I5 <- if((L3-17.3)<a*(H-17.3)) I5 <- 1 else(I5 <- 0)
	I6 <- if((U3-17.3)<a*(H-17.3)) I6 <- 1 else(I6 <- 0)

	### main stem volume calculation ################
	V<- 0.005454154*(I1*D^2*((1-G*W)*(U1-L1)+W*((1-L1/H)^r*(H-L1) -
	(1-U1/H)^r*(H-U1))/(r+1))
	+ I2*I3*(T*(U2-L2)+Z*((1-L2/H)^p*(H-L2) -
	(1-U2/H)^p*(H-U2))/(p+1))
	+ I4*F^2*(b*(U3-L3)-b*((U3-17.3)^2-(L3-17.3)^2)/(H-17.3) +
	(b/3)*((U3-17.3)^3-(L3-17.3)^3)/(H-17.3)^2 +
	I5*(1/3)*((1-b)/a^2)*(a*(H-17.3)-(L3-17.3))^3/(H-17.3)^2 -
	I6*(1/3)*((1-b)/a^2)*(a*(H-17.3)-(U3-17.3))^3/(H-17.3)^2))
	return( V*0.0283168466 )
}

ETfunction <- function(){

meah=10

Tmean2007_rasterbrick <- (tmmx2007brick - tmmn2007brick)/2
RHmean2007_rasterbrick <- (rmax2007brick - rmin2007brick)/2
es2007 <- 0.6108 * exp(17.27 * Tmean2007_rasterbrick / (Tmean2007_rasterbrick+237.3))
ea2007 <- (RHmean2007_rasterbrick / 100) * es2007
deltaVP <- es2007 - ea2007
vappressure2007 <- ea2007
z = 1000

ETlatradians <- radians(ETlat_array)
#extraT = extrat(dayOfYear(dayspan), ETlatradians)$ExtraTerrestrialSolarRadiationDaily
#tal <- cst(srad2007brick, dayOfYear(dayspan), radians(ETlatradians))
extraT = 50
tal = 1

erichet2007 <- (0.408 * deltaVP(tmmx2007brick, tmmn2007brick) * 
                  (rns(srad2007brick, albedo = 0.23) - rnl(tmmx2007brick, tmmn2007brick, 
                                                           srad2007brick, vappressure2007, extraT, tal)) + psychC(tmmx2007brick, 
                                                                                                                  tmmn2007brick, z) * (900/(Tmean2007_rasterbrick + 273)) * 
                  wind2(vs2007brick, meah) * (es(tmmx2007brick,tmmn2007brick) - 
                                                vappressure2007))/(deltaVP(tmmx2007brick, tmmn2007brick) + psychC(tmmx2007brick, 
                                                                                                                  tmmn2007brick, z) * (1 + 0.34 * wind2(vs2007brick, meah)))

erichet2007_mean <- calc(erichet2007, mean)
et2007m <- as.matrix(erichet2007)

return()
}
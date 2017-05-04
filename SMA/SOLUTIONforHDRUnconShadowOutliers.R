

setwd("C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/MESMA/Viper/20170227_TestRun")

xx <- raster("Laikipia_mesma_unconstrained_20170227_shadeNorm", band = 1)


xx.mean <- cellStats(xx, mean)
xx.std <- cellStats(xx, sd)


xx[xx > xx.mean + 3*xx.std] <- NA ## This will get rid of the hdr unconstrained shadow normalized problem if applied after loading rasters
xx[xx < xx.mean - 3*xx.std] <- NA # would need to go in the addSMA function


xxmin <- cellStats(xx, min)
xxmax <- cellStats(xx, max)
hist(xx)


plot(xx)





# Run these commands if you're missing any required packages.
install.packages('gstat')
install.packages('lattice')
install.packages('maps')
install.packages('maptools')
install.packages('MASS')
install.packages('sp')
install.packages('spdep')
install.packages('spatstat')

# Load all the libraries we'll use during this presentation.
library(gstat)
library(lattice)
library(maps)
library(maptools)
library(MASS)
library(sp)
library(spdep)
library(spatstat)

# We'll start with basic spatial data objects by using the Meuse data set.
data(meuse)
class(meuse)
head(meuse)

# Review the default plot() output for data frames.
jpeg('images/Meuse_Scatterplot.jpg', width = 800, height = 600)
plot(meuse)
dev.off()

# Adding coordinates redefines this a SpatialPointsDataFrame.
coordinates(meuse) <- c('x', 'y')
class(meuse)

# See how plot() works for a SpatialPointsDataFrame.
jpeg('images/Meuse_Point_Plot.jpg', width = 800, height = 600)
plot(meuse)
title('The Meuse Data - Point Process Plot')
dev.off()

# If you want additional information, use spplot with a z dimension.
jpeg('images/Meuse_Zinc_Plot.jpg', width = 800, height = 600)
spplot(meuse, 'zinc', main = 'The Meuse Data - Zinc Distribution')
dev.off()

# On a logarithmic scale.
jpeg('images/Meuse_Log_Zinc_Plot.jpg', width = 800, height = 600)
spplot(meuse, 'zinc', do.log = TRUE, main = 'The Meuse Data - Log Zinc Distribution')
dev.off()

# Using bubbles instead of colors.
jpeg('images/Meuse_Zinc_Bubble_Plot.jpg', width = 800, height = 600)
bubble(meuse, 'zinc', main = 'The Meuse Data - Zinc Bubble Plot')
dev.off()

# Plot some SpatialLines instead of SpatialPoints.
cc <- coordinates(meuse)
m.sl <- SpatialLines(list(Lines(list(Line(cc)))))
jpeg('images/Meuse_Line_Plot.jpg', width = 800, height = 600)
plot(m.sl)
title('The Meuse Data - Line Plot')
dev.off()

# Import information about the river's boundaries as SpatialPolygons.
data(meuse.riv)
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), 'meuse.riv'))
meuse.sr <- SpatialPolygons(meuse.lst)
jpeg('images/Meuse_Polygon_Plot.jpg', width = 800, height = 600)
plot(meuse.sr, col = 'grey')
title('The Meuse River - Polygon Plot')
dev.off()

# Work with a grid of SpatialPixels.
data(meuse.grid)
coordinates(meuse.grid) <- c('x', 'y')
meuse.grid <- as(meuse.grid, "SpatialPixels")
jpeg('images/Meuse_Grid_Plot.jpg', width = 800, height = 600)
image(meuse.grid, col = 'grey')
title('The Meuse River - Grid Plot')
dev.off()

# Now combine everything into a single plot.
jpeg('images/Meuse_Pooled_Plot.jpg', width = 800, height = 600)
image(meuse.grid, col = 'grey')
plot(meuse.sr, col = 'grey', add = TRUE)
plot(meuse, add = TRUE)
title('The Meuse River - All Data')
dev.off()

# Basic data.
home.directory <- '/Users/johnmyleswhite/Spatial Statistics Talk'
setwd(home.directory)
load('Datasets.Rdata')

# Use the election data set from Yuri's data.
class(election)

# Do a basic polygon plot to see his data.
jpeg('images/Election_Data_-_Raw_Polygons.jpg', width = 800, height = 600)
plot(election)
title('Election Data - Raw Polygons')
dev.off()

# Now add colors for votes.
jpeg('images/Election_Data_-_Binary_Coloring.jpg', width = 800, height = 600)
plot(election,
     col = with(as.data.frame(election), ifelse(Bush > Kerry, 'red', 'blue')),
     border = NA)
title('Election Data - Binary Coloring')
dev.off()

# Then add borders.
jpeg('images/Election_Data_-_Binary_Coloring_with_Borders.jpg', width = 800, height = 600)
plot(election,
     col = with(as.data.frame(election), ifelse(Bush > Kerry, 'red', 'blue')),
     border = 'black')
title('Election Data - Binary Coloring with Borders')
dev.off()

# Continuous vote visualization.
jpeg('images/Election_Data_-_Continuous_Coloring.jpg', width = 800, height = 600)
br.palette <- colorRampPalette(c('blue', 'red'), space = 'rgb')
spplot(election,
       zcol = 'Bush_pct',
       col.regions = br.palette(100),
       main = 'Election Data - Continuous Coloring with Borders')
dev.off()

# Print out for the introductory image.
jpeg('images/spplot_Election_Map.jpg', width = 800, height = 600)
br.palette <- colorRampPalette(c('blue', 'red'), space = 'rgb')
spplot(election,
       zcol = 'Bush_pct',
       col.regions = br.palette(100))
dev.off()
       
# Volcano grid dataset. (UNUSED IN PRESENTATION)
class(volcano)
image(x = 10 * (1:nrow(volcano)),
      y = 10 * (1:ncol(volcano)),
      z = volcano,
      col = terrain.colors(100),
      axes = FALSE)
contour(x = 10 * (1:nrow(volcano)),
        y = 10 * (1:ncol(volcano)),
        z = volcano,
        levels = seq(from = min(volcano),
                     to = max(volcano),
                     by = 10),
                     axes = FALSE,
                     add = TRUE)
persp(volcano,
      col = terrain.colors(100))

# Basic example of a Poisson Process
hpp <- rpoispp(100)
class(hpp)
str(hpp)
hpp$x
hpp$y
plot(hpp)

# Various intensity examples.
intensities <- seq(0, 80, by = 20)
for (intensity in intensities)
{
	hpp <- rpoispp(intensity)
	filename <- paste('HPP_with_Intensity_', intensity, '.jpg', sep = '')
	jpeg(filename)
	plot(hpp)
	dev.off()
}

# IPP example with distance intensity function.
jpeg('images/IPP_Example.jpg', width = 800, height = 600)
ipp <- rpoispp(function(x,y) {intensity * (x^2 + y^2)})
plot(ipp)
dev.off()

# Alternative intensity function.
jpeg('images/IPP_Example_2.jpg', width = 800, height = 600)
ipp <- rpoispp(function(x,y) { intensity *(1 / (1.001 - x^2) + 1 / (1.001 - y^2)) })
plot(ipp)
dev.off()

# Simulate a Neyman-Scott process
jpeg('images/NSPP_Example.jpg', width = 800, height = 600)
nspp <- rNeymanScott(kappa = 10,
                     rmax = 0.1,
                     function(x,y)
                     {
                       runifdisc(5, 0.1, centre = c(x, y))
                     })
plot(nspp)
dev.off()

# Another type of data set with repulsion effects.
data(cells)
jpeg('images/Cells_Data.jpg', width = 800, height = 600)
plot(cells)
dev.off()

# Possibly HPP data.
data(japanesepines)
jpeg('images/Japanese_Pines_Data.jpg', width = 800, height = 600)
plot(japanesepines)
dev.off()

# Naturally clustered data.
data(redwood)
jpeg('images/Redwood_Data.jpg', width = 800, height = 600)
plot(redwood)
dev.off()

# Tests for CSR or systematic deviations from it.
# Gest()
jpeg('images/Gest_HPP.jpg', width = 800, height = 600)
plot(Gest(hpp))
dev.off()

jpeg('images/Gest_IPP.jpg', width = 800, height = 600)
plot(Gest(ipp))
dev.off()

jpeg('images/Gest_NSPP.jpg', width = 800, height = 600)
plot(Gest(nspp))
dev.off()

jpeg('images/Gest_Cells.jpg', width = 800, height = 600)
plot(Gest(cells))
dev.off()

jpeg('images/Gest_Japanese_Pines.jpg', width = 800, height = 600)
plot(Gest(hpp))
dev.off()

jpeg('images/Gest_Redwood.jpg', width = 800, height = 600)
plot(Gest(redwood))
dev.off()

# Fest()
jpeg('images/Fest_HPP.jpg', width = 800, height = 600)
plot(Fest(hpp))
dev.off()

jpeg('images/Fest_IPP.jpg', width = 800, height = 600)
plot(Fest(ipp))
dev.off()

jpeg('images/Fest_NSPP.jpg', width = 800, height = 600)
plot(Fest(nspp))
dev.off()

jpeg('images/Fest_Cells.jpg', width = 800, height = 600)
plot(Fest(cells))
dev.off()

jpeg('images/Fest_Japanese_Pines.jpg', width = 800, height = 600)
plot(Fest(hpp))
dev.off()

jpeg('images/Fest_Redwood.jpg', width = 800, height = 600)
plot(Fest(redwood))
dev.off()

# Kest()
jpeg('images/Kest_HPP.jpg', width = 800, height = 600)
plot(Kest(hpp))
dev.off()

jpeg('images/Kest_IPP.jpg', width = 800, height = 600)
plot(Kest(ipp))
dev.off()

jpeg('images/Kest_NSPP.jpg', width = 800, height = 600)
plot(Kest(nspp))
dev.off()

jpeg('images/Kest_Cells.jpg', width = 800, height = 600)
plot(Kest(cells))
dev.off()

jpeg('images/Kest_Japanese_Pines.jpg', width = 800, height = 600)
plot(Kest(hpp))
dev.off()

jpeg('images/Kest_Redwood.jpg', width = 800, height = 600)
plot(Kest(redwood))
dev.off()

# Use the envelope() function for building statistical tests.

# Re-import Meuse data for spatial autocorrelation example.
data(meuse)
coordinates(meuse) <- c('x', 'y')
spplot(meuse, 'zinc', do.log = TRUE)
bubble(meuse, 'zinc', do.log = TRUE, key.space = 'bottom')

jpeg('images/XY_Plot_Example.jpg', width = 800, height = 600)
xyplot(log(zinc) ~ sqrt(dist), as.data.frame(meuse))
dev.off()

zn.lm <- lm(log(zinc) ~ sqrt(dist), data = as.data.frame(meuse))
meuse$fitted.s <- predict(zn.lm, meuse) - mean(predict(zn.lm, meuse))
meuse$residuals <- residuals(zn.lm)
jpeg('images/Spatial_Correlation_Example.jpg', width = 800, height = 600)
spplot(meuse, c('fitted.s', 'residuals'))
dev.off()

# Plot the empirical variogram.
jpeg('images/Empirical_Variogram.jpg', width = 800, height = 600)
plot(variogram(zinc ~ 1, meuse))
dev.off()

# Randomly permute values, compare this to earlier variogram.
jpeg('images/Permuted_Variogram.jpg', width = 800, height = 600)
meuse$permuted.zinc <- sample(meuse$zinc, nrow(meuse))
plot(variogram(permuted.zinc ~ 1, meuse))
dev.off()

# IDW Interpolation.
data(meuse)
data(meuse.riv)
data(meuse.grid)
coordinates(meuse) <- c('x', 'y')
coordinates(meuse.riv) <- c('x', 'y')
coordinates(meuse.grid) <- c('x', 'y')
meuse.grid <- as(meuse.grid, 'SpatialPixelsDataFrame')

idw.out <- idw(zinc ~ 1, meuse, meuse.grid, idp = 2.5)

jpeg('images/Zinc_Refresher.jpg', width = 800, height = 600)
spplot(meuse, z = 'zinc')
dev.off()

jpeg('images/Zinc_IDW_Predictions.jpg', width = 800, height = 600)
spplot(idw.out, z = 'var1.pred')
dev.off()

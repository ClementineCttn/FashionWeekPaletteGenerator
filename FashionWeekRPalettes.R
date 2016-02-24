############# Crafty Palette

SS16 = c("#77c5ba", "#bbab61", "#ffced0", "#2db92d", "#9c636f", 
         "#ff9000", "#e1ff2f", "#505050", "#937f66", "#ff2313")
plot(1:10 ~ 1, col = SS16, cex= sort(1:10, decreasing = T), pch=15, axes=FALSE))
title("My Spring/Summer '16")
 

############# Geeky Palette from the London Fashion Week Press Reports
# Adaptation from the code retrieved here: https://gist.githubusercontent.com/dsparks/3980277/raw/56443bed3d3aa423ae5ffcbe8bdae22dc5fdfda4/kmeans_palette.R
doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("jpeg", "reshape2", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
imageLoader <- function(name){  # This function takes a URL, and generates a data.frame with pixel locations and colors
  readImage <- readJPEG(name)  
  longImage <- melt(readImage)
  rgbImage <- reshape(longImage, timevar = "Var3",
                      idvar = c("Var1", "Var2"), direction = "wide")
  rgbImage$Var1 <- -rgbImage$Var1
  return(rgbImage)
}

# Zip files containing the pictures here: http://www.londonfashionweek.co.uk/News/Press-Portal?y=2016
kColors <- 10  # Number of palette colors

d = 2
Day = paste0("London_AW16/London_AW16_Day", d, "/")
pics = list.files(pattern="*.jpg", path = Day)

pal = data.frame(Hexa = NULL, Var1 = NULL, Freq = NULL)
for (p in 1:length(pics)){
  print(paste0(p,'/',length(pics)))
  rgbImage <- imageLoader(paste0(Day, pics[p]))
kMeans <- kmeans(rgbImage[, 3:5], centers = kColors)
clusters = data.frame(Hexa = rgb(kMeans$centers), table(kMeans$cluster))
pal = rbind(pal, clusters)
}
#pal

pal$Hexa = as.character(pal$Hexa)
pal = pal[order(-pal$Freq),]
SelectPal = pal[1:kColors,]
barplot(pal$Freq, col = pal$Hexa, axes = F,
        ylab="", space = 0.5, las = 1, border = F)
barplot(SelectPal$Freq, col = SelectPal$Hexa, axes = F,
        ylab="", space = 0.5, las = 1, border = F)


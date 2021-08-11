Imports:
  grDevices
  ggplot2

#Defining the colour palette with AFRY's standard scheme
ColorPalette <- c("#08519c","#6baed6","#c6dbef","#00441b","#41ab5d","#c7e9c0","#543005","#bf812d","#f6e8c3",
  "#737373")

Red <- grDevices::col2rgb(ColorPalette)[1,]
Green <- grDevices::col2rgb(ColorPalette)[2,]
Blue <- grDevices::col2rgb(ColorPalette)[3,]

#Defining Function colour_afry
colour_afry <- function (n, name = c("AFRYPalette"))
{
  AFRYPalette = grDevices::rgb(Red, Green, Blue, maxColorValue = 255)
  name = match.arg(name)
  orig = eval(parse(text = name))
  rgb = t(grDevices::col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, , length(orig))
  xg = seq(0, 1, , n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = grDevices::rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}

#Defining function theme_afry
theme_afry <- function(){
  ggplot2::theme_bw() %+replace%
    ggplot2::theme(panel.grid = element_line(colour = "grey75"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major.x = element_blank(),
                   legend.position="none",
                   axis.line=element_line (colour="grey75"),
                   panel.border = element_blank(),
                   text=element_text(size=30),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line (colour="grey75"),
                   axis.ticks.length=unit(0.4,"cm"),
                   axis.text.x = element_text(angle = 45, hjust=0.5))
}


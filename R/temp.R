library(tidyverse)
library(ggplot2)
library(ggpattern)
library(ggpubr)
library(sp)
library(polyclip)
library(latex2exp)



poly <- tribble(
        ~x, ~y, ~holes, ~id,
        0.1802,0.8712, 1L, 1L,
        0.2573,0.7795, 1L, 2L,
        0.0331,0.6281, 1L, 3L,
        0.2457,0.6223, 1L, 4L,
        0.2806,0.5088, 1L, 5L,
        0.3723,0.6136, 1L, 6L,
        0.3621,0.3996, 1L, 7L,
        0.4306,0.3297, 1L, 8L,
        0.5266,0.3195, 1L, 9L,
        0.5630,0.0677, 1L, 10L,
        0.8629,0.2103, 1L, 11L,
        0.9401,0.5189, 1L, 12L,
        0.7086,0.4942, 1L, 13L,
        0.5892,0.6529, 1L, 14L,
        0.6693,0.8014, 1L, 15L,
        0.3985,0.9382, 1L, 16L,
        0.6000,0.4000, 2L, 17L,
        0.6000,0.2000, 2L, 18L,
        0.8000,0.2000, 2L, 19L,
        0.8000,0.4000, 2L, 20L
)

poly_small <- tribble(
        ~x, ~y,
        0.84578829, 0.22430933,
        0.91374870, 0.49597492,
        0.69943962, 0.47310911,
        0.56551379, 0.65111605,
        0.64194392, 0.79281236,
        0.39666975, 0.91671748,
        0.21495193, 0.86094518,
        0.28690924, 0.77536172,
        0.09592715, 0.64639344,
        0.26059704, 0.64190103,
        0.28917259, 0.54896909,
        0.36660303, 0.63746102,
        0.39295365, 0.62683747,
        0.38249282, 0.40736516,
        0.43985652, 0.34882907,
        0.54417031, 0.33774573,
        0.57891272, 0.09741218,
        0.75260197, 0.18000000,
        0.58000000, 0.18000000,
        0.58000000, 0.42000000,
        0.82000000, 0.42000000,
        0.82000000, 0.21204721,
)

poly_plein <- tribble(
        ~x, ~y, ~holes, ~id,
        0.1802,0.8712, 1L, 1L,
        0.2573,0.7795, 1L, 2L,
        0.0331,0.6281, 1L, 3L,
        0.2457,0.6223, 1L, 4L,
        0.2806,0.5088, 1L, 5L,
        0.3723,0.6136, 1L, 6L,
        0.3621,0.3996, 1L, 7L,
        0.4306,0.3297, 1L, 8L,
        0.5266,0.3195, 1L, 9L,
        0.5630,0.0677, 1L, 10L,
        0.8629,0.2103, 1L, 11L,
        0.9401,0.5189, 1L, 12L,
        0.7086,0.4942, 1L, 13L,
        0.5892,0.6529, 1L, 14L,
        0.6693,0.8014, 1L, 15L,
        0.3985,0.9382, 1L, 16L
)

poly_trou <- tribble(
        ~x, ~y, ~holes, ~id,
        0.6000,0.4000, 2L, 17L,
        0.6000,0.2000, 2L, 18L,
        0.8000,0.2000, 2L, 19L,
        0.8000,0.4000, 2L, 20L
)

c_poly_plein <- poly_plein %>% 
        slice(chull(x,y))

b1 <- poly[c(1:3),]
b3 <- poly[c(3:10),]
b2 <- poly[c(12,13,14,15),]

delta <- 0.02
xmin <- 0
xmax <- 1
ymin <- 0
ymax <- 1
mygrid <- as_tibble(expand.grid(x = seq(xmin,xmax,delta), y = seq(ymin,ymax,delta)))
mygrid <- mygrid %>% 
        mutate(
                xbl = x - delta/2,
                ybl = y - delta/2,
                xbr = x + delta/2,
                ybr = y - delta/2,
                xtl = x - delta/2,
                ytl = y + delta/2,
                xtr = x + delta/2,
                ytr = y + delta/2,
        )

mygrid <- mygrid %>% 
        mutate(
                inblp1 = point.in.polygon(mygrid$xbl,mygrid$ybl,poly_plein$x,poly_plein$y),
                inbrp1 = point.in.polygon(mygrid$xbr,mygrid$ybr,poly_plein$x,poly_plein$y),
                intlp1 = point.in.polygon(mygrid$xtl,mygrid$ytl,poly_plein$x,poly_plein$y),
                intrp1 = point.in.polygon(mygrid$xtr,mygrid$ytr,poly_plein$x,poly_plein$y),
                inblp2 = point.in.polygon(mygrid$xbl,mygrid$ybl,poly_trou$x,poly_trou$y),
                inbrp2 = point.in.polygon(mygrid$xbr,mygrid$ybr,poly_trou$x,poly_trou$y),
                intlp2 = point.in.polygon(mygrid$xtl,mygrid$ytl,poly_trou$x,poly_trou$y),
                intrp2 = point.in.polygon(mygrid$xtr,mygrid$ytr,poly_trou$x,poly_trou$y),
                in_out = (inblp1 & !inblp2) & (inbrp1 & !inbrp2) & (intlp1 & !intlp2) & (intrp1 & !intrp2)
        )
ggplot() +
        aes(x=x,y=y)+
        geom_tile(aes(fill=in_out),data = mygrid, colour = 'gray90')+
        scale_fill_manual(breaks = c(FALSE, TRUE), 
                          values=c("white", "#5ec962"))+
        geom_polygon(
                data=poly,
                fill = "#3b528b",
                aes(subgroup=holes),
                color = "black", 
                alpha = 0,
                size=1
        )+
        geom_point(data=poly, size=3)+
        geom_vline(xintercept=0.5)+
        geom_vline(xintercept=0.25)+
        geom_hline(yintercept=0.5)+
        geom_hline(yintercept=0.75)+
        theme(legend.position="none")



A <- list(list(x = poly_plein$x, y = poly_plein$y))
B <- list(list(x = poly_trou$x, y = poly_trou$y))

A_small <- polyoffset(A, -0.02, jointype="miter")
B_big <- polyoffset(B, 0.02, jointype = "miter")
plot(c(0,1),c(0,1), type="n",axes=TRUE, xlab="", ylab="")
polygon(A[[1]], col="grey")
polygon(A_small[[1]], col="grey")
polygon(B_big[[1]], col="grey")

ggplot() +
        aes(x=x,y=y)+
        geom_tile(aes(fill=in_out),data = mygrid, colour = 'gray90')+
        scale_fill_manual(breaks = c("0", "1"), 
                          values=c("white", "#5ec962"))+
        geom_point(data=poly, size=3)+
        theme_void()






ggplot() +
        aes(x=x,y=y)+
        geom_polygon_pattern(
                data = square,
                pattern = "crosshatch",
                pattern_angle = 0,
                pattern_spacing = 0.02,
                pattern_color = "gray",
                pattern_fill = "gray",
                pattern_size = 0.05,
                pattern_density = 0.05,
                fill = "white",
                color = "black"
        )+
        geom_polygon(
                data=poly,
                fill = "#3b528b",
                aes(subgroup=holes),
                color = "black", 
                alpha = 0.5,
                size=1.5
        )+
        geom_point(data=poly, size=5)+
        theme_void()



ggplot() +
        aes(x=x,y=y)+
        geom_polygon_pattern(
                data = square,
                pattern = "crosshatch",
                pattern_angle = 0,
                pattern_spacing = 0.02,
                pattern_color = "gray",
                pattern_fill = "gray",
                pattern_size = 0.05,
                pattern_density = 0.05,
                fill = "white",
                color = "black"
        )+
        geom_polygon_pattern(
                data=poly,
                pattern = "crosshatch",
                pattern_spacing = 0.02,
                pattern_color = "green",
                pattern_fill = "green",
                fill = "white",
                aes(subgroup=holes),
                color = "black", 
                alpha = 0.5,
                size=1.5
        )+
        geom_point(data=poly, size=5)+
        theme_void()




ggplot(mygrid, aes(x, y)) + 
        geom_tile(colour = 'gray90', fill = "white")+
        theme_void()












mygrid <- as_tibble(expand.grid(x = seq(xmin,xmax,delta), y = seq(ymin,ymax,delta)))
mygrid <- mygrid %>% 
        mutate(
                xbl = x - delta/2,
                ybl = y - delta/2,
                xbr = x + delta/2,
                ybr = y - delta/2,
                xtl = x - delta/2,
                ytl = y + delta/2,
                xtr = x + delta/2,
                ytr = y + delta/2,
        )

mygrid <- mygrid %>% 
        mutate(
                inblp = point.in.polygon(mygrid$xbl,mygrid$ybl,poly_small$x,poly_small$y),
                inbrp = point.in.polygon(mygrid$xbr,mygrid$ybr,poly_small$x,poly_small$y),
                intlp = point.in.polygon(mygrid$xtl,mygrid$ytl,poly_small$x,poly_small$y),
                intrp = point.in.polygon(mygrid$xtr,mygrid$ytr,poly_small$x,poly_small$y),
                in_out = (inblp) & (inbrp) & (intlp) & (intrp)
        ) %>% 
        mutate(
                in_out = case_when(
                        in_out == FALSE ~ "0",
                        in_out == TRUE ~ "1"
                )
        ) %>% 
        rowid_to_column("id") %>% 
        select(id, x, y, in_out)

for (i in seq(1,nrow(mygrid))){
  if (mygrid[i,]$in_out == "1"){
    w  <- mygrid[i-1,]$in_out
    e  <- mygrid[i+1,]$in_out
    sw <- mygrid[i-1-51,]$in_out
    s  <- mygrid[i-51,]$in_out
    se <- mygrid[i+1-51,]$in_out
    nw <- mygrid[i-1+51,]$in_out
    n  <- mygrid[i+51,]$in_out
    ne <- mygrid[i+1+51,]$in_out
    test <- (w=="1")&&(e=="1")&&(s=="1")&&(n=="1")&&(sw=="1")&&(se=="1")&&(nw=="1")&&(ne=="1")
    if (test==FALSE){
            mygrid[i,]$in_out <- "2"
    }
  }
}



square <- tribble(
  ~x, ~y,
  0, 0,
  1, 0,
  1, 1,
  0, 1
)

ggplot() +
  aes(x=x,y=y)+
  geom_polygon_pattern(
    data = square,
    pattern = "crosshatch",
    pattern_angle = 45,
    pattern_spacing = 0.035,
    pattern_color = "gray",
    pattern_fill = "gray",
    pattern_size = 0.05,
    pattern_density = 0.05,
    fill = "white",
    color = "black"
  )+
  geom_polygon(
    data=poly,
    aes(subgroup=holes),
    color = "black", 
    fill = "#3b528b",
    alpha = 0,
    size=1.5
  )+
  geom_polygon(
    data=poly_small,
    color = "green", 
    fill = "#3b528b",
    alpha = 0,
    size=1.5
  )+
  geom_abline(aes(intercept=0.89,slope=-1),color ="red",size=1.5)+
  geom_point(data=poly, size=5)+
  geom_point(aes(x=0.185,y=0.705),color="blue",size=3)+
  geom_point(aes(x=0.25,y=0.64),color="blue",size=3)+
  geom_point(aes(x=0.265,y=0.625),color="blue",size=3)+
  geom_point(aes(x=0.315,y=0.575),color="blue",size=3)+
  theme_void()+
  coord_fixed(ratio = 1)



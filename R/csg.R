library(tidyverse)
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

b1 <- poly[c(1:3),]
b3 <- poly[c(3:10),]
b2 <- poly[c(12,13,14,15),]

c1 <- poly[c(4,5,6),]
c2 <- poly[c(6,7,8,9),]

c_b3 <- b3 %>% 
  slice(chull(x,y))

not_c_b3 <- b3 %>% 
  slice(-chull(x,y))
  
p1 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(data=poly,aes(subgroup=holes),color = "black", fill = "#3b528b",alpha = 0.5)+
  geom_point(data=poly)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank())
  
c_poly_plein <- poly_plein %>% 
  slice(chull(x,y))

not_c_poly_plein <- poly_plein %>% 
  slice(-chull(x,y))

p1 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly,
    aes(subgroup=holes),
    color = "black", 
    fill = "#3b528b",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.4, y=0.8, label=TeX("$P_1$"), size=12)+
  annotate("text", x=0.7, y=0.3, label=TeX("$P_2$"), size=12)+
  geom_point(data=poly, size=5)+
  theme_void()

p2 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly_plein,
    aes(subgroup=holes),
    color = "black", 
    fill = "#3b528b",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.4, y=0.8, label=TeX("$P_1$"), size=12)+
  geom_point(data=poly_plein, size=5)+
  theme_void()

p3 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly_plein,
    aes(subgroup=holes),
    color = "black", 
    fill = "#3b528b",
    alpha = 0.5,
    size=1.5
  )+
  geom_polygon(
    data=c_poly_plein,
    aes(subgroup=holes),
    color = "black", 
    fill = "#5ec962",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.4, y=0.8, label=TeX("$A$"), size=12)+
  geom_point(data=poly_plein, size=5)+
  theme_void()

p4 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly_plein,
    aes(subgroup=holes),
    color = "black", 
    fill = "#3b528b",
    alpha = 0.5,
    size=1.5
  )+
  geom_polygon(
    data=b1,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  geom_polygon(
    data=b2,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  geom_polygon(
    data=b3,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.4, y=0.8, label=TeX("$P_1$"), size=12)+
  annotate("text", x=0.1709, y=0.7677, label=TeX("$B_1$"), size=6)+
  annotate("text", x=0.3009, y=0.4416, label=TeX("$B_3$"), size=6)+
  annotate("text", x=0.7272, y=0.6228, label=TeX("$B_2$"), size=6)+
  geom_point(data=poly_plein, size=5)+
  theme_void()

p5 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly_plein,
    aes(subgroup=holes),
    color = "gray", 
    fill = "#3b528b",
    alpha = 0,
    size=1.5
  )+
  geom_polygon(
    data=b1,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  geom_polygon(
    data=b2,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  geom_polygon(
    data=b3,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.1709, y=0.7677, label=TeX("$B_1$"), size=6)+
  annotate("text", x=0.3009, y=0.4416, label=TeX("$B_3$"), size=6)+
  annotate("text", x=0.7272, y=0.6228, label=TeX("$B_2$"), size=6)+
  geom_point(data=poly_plein, size=5)+
  theme_void()

p6 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly_plein,
    aes(subgroup=holes),
    color = "gray", 
    fill = "#3b528b",
    alpha = 0,
    size=1.5
  )+
  geom_polygon(
    data=b3,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.3009, y=0.4416, label=TeX("$B_3$"), size=12)+
  geom_point(data=poly_plein, size=5)+
  theme_void()
  
p7 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly_plein,
    aes(subgroup=holes),
    color = "gray", 
    fill = "#3b528b",
    alpha = 0,
    size=1.5
  )+
  geom_polygon(
    data=c_b3,
    color = "black", 
    fill = "#5ec962",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.3009, y=0.4416, label=TeX("$C$"), size=12)+
  geom_point(data=poly_plein, size=5)+
  theme_void()
  
p8 <- ggplot() +
  aes(x=x,y=y)+
  geom_polygon(
    data=poly_plein,
    aes(subgroup=holes),
    color = "gray", 
    fill = "#3b528b",
    alpha = 0,
    size=1.5
  )+
  geom_polygon(
    data=c1,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  geom_polygon(
    data=c2,
    color = "black", 
    fill = "#fde725",
    alpha = 0.5,
    size=1.5
  )+
  annotate("text", x=0.3009, y=0.58, label=TeX("$D_1$"), size=6)+
  annotate("text", x=0.43, y=0.4, label=TeX("$D_2$"), size=6)+
  geom_point(data=poly_plein, size=5)+
  theme_void()

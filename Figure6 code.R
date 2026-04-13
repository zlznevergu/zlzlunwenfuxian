#========================
# 1. 安装并加载包
#========================
if (!require(fmsb)) install.packages("fmsb")
library(fmsb)

#========================
# 2. 左图数据
# 顺序：SPF1, SPF2, SPF3, SPF4
#========================
df_left <- data.frame(
  SPF1 = c(1, 0, 0.45, 0.55, 0.50, 0.30),
  SPF2 = c(1, 0, 0.55, 0.62, 0.40, 0.50),
  SPF3 = c(1, 0, 0.55, 0.70, 0.15, 0.45),
  SPF4 = c(1, 0, 0.50, 0.78, 0.30, 0.15)
)
rownames(df_left) <- c("max", "min", "Grassland", "Forestland", "Cropland", "Orchard")

#========================
# 3. 右图数据
#========================
df_right <- data.frame(
  SPF1 = c(1, 0, 0.50, 0.68, 0.58, 0.35, 0.30),
  SPF2 = c(1, 0, 0.43, 0.50, 0.47, 0.60, 0.65),
  SPF3 = c(1, 0, 0.10, 0.10, 0.20, 0.45, 0.50),
  SPF4 = c(1, 0, 0.29, 0.42, 0.40, 0.45, 0.50)
)
rownames(df_right) <- c("max", "min", "0Y(Cropland)", "5Y", "15Y", "25Y", "35Y")

#========================
# 4. 输出到窗口
#========================
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2), xpd = TRUE, family = "serif")

#------------------------
# 左图
#------------------------
radarchart(
  df_left,
  axistype = 1,
  seg = 5,
  pcol = c("#9ACD32", "#33A02C", "red", "#F4A62A"),
  pfcol = NA,
  plwd = 2,
  plty = 1,
  cglcol = "grey70",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "black",
  vlcex = 1.2,
  vlabels = c(expression(SPF[1]), expression(SPF[2]),
              expression(SPF[3]), expression(SPF[4]))
)

legend("topleft",
       legend = c("Grassland", "Forestland", "Cropland", "Orchard"),
       col = c("#9ACD32", "#33A02C", "red", "#F4A62A"),
       pch = c(15, 16, 17, 25),
       pt.bg = c("#9ACD32", "#33A02C", "red", "#F4A62A"),
       lty = 1, lwd = 1.3, bty = "n", cex = 0.95,
       inset = c(-0.12, 0.16))

mtext("(a)", side = 3, adj = -0.08, line = 1, cex = 1.1)

#------------------------
# 右图
#------------------------
radarchart(
  df_right,
  axistype = 1,
  seg = 5,
  pcol = c("red", "#1565FF", "#00A651", "grey60", "#A65EEA"),
  pfcol = NA,
  plwd = 2,
  plty = 1,
  cglcol = "grey70",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "black",
  vlcex = 1.2,
  vlabels = c(expression(SPF[1]), expression(SPF[2]),
              expression(SPF[3]), expression(SPF[4]))
)

legend("topleft",
       legend = c("0Y(Cropland)", "5Y", "15Y", "25Y", "35Y"),
       col = c("red", "#1565FF", "#00A651", "grey60", "#A65EEA"),
       pch = c(15, 16, 17, 25, 18),
       pt.bg = c("red", "#1565FF", "#00A651", "grey60", "#A65EEA"),
       lty = 1, lwd = 1.3, bty = "n", cex = 0.95,
       inset = c(-0.10, 0.16))

mtext("(b)", side = 3, adj = -0.08, line = 1, cex = 1.1)

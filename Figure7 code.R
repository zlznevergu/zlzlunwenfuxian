#========================
# 1. 加载包
#========================
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(patchwork)) install.packages("patchwork")

library(ggplot2)
library(patchwork)

#========================
# 2. 图(a)数据：不同恢复类型
# 近似值按图片读取
#========================
df_a <- data.frame(
  group = c("CL", "CTO", "CTG", "CTF"),
  mean  = c(0.34, 0.45, 0.49, 0.63),
  se    = c(0.06, 0.05, 0.04, 0.17),
  letter = c("c", "bc", "b", "a"),
  fill = c("#F4003A", "#EDAE5D", "#9BE000", "#69BE3C")
)

df_a$group <- factor(df_a$group, levels = c("CL", "CTO", "CTG", "CTF"))

#========================
# 3. 图(b)数据：不同恢复年限
#========================
df_b <- data.frame(
  group = c("0Y(CL)", "5Y", "15Y", "25Y", "35Y"),
  mean  = c(0.34, 0.41, 0.44, 0.45, 0.42),
  se    = c(0.06, 0.11, 0.13, 0.05, 0.06),
  letter = c("a", "a", "a", "a", "a"),
  fill = c("#9DC4AF", "#84B08E", "#4E8A54", "#356F39", "#6AA179")
)

df_b$group <- factor(df_b$group, levels = c("0Y(CL)", "5Y", "15Y", "25Y", "35Y"))

#========================
# 4. 通用主题
#========================
theme_fig <- theme_classic(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.margin = margin(10, 10, 10, 10)
  )

#========================
# 5. 图(a)
#========================
p_a <- ggplot(df_a, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.08, linewidth = 0.4) +
  geom_text(aes(y = mean + se + 0.03, label = letter), size = 5) +
  scale_fill_manual(values = setNames(df_a$fill, df_a$group)) +
  scale_y_continuous(limits = c(0, 0.9), breaks = seq(0, 0.8, 0.2)) +
  labs(y = "SPQI") +
  annotate("text", x = 0.7, y = 0.82, label = "(a)", size = 6) +
  theme_fig +
  theme(legend.position = "none")

#========================
# 6. 图(b)
#========================
p_b <- ggplot(df_b, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.5) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.08, linewidth = 0.4) +
  geom_text(aes(y = mean + se + 0.03, label = letter), size = 5) +
  scale_fill_manual(values = setNames(df_b$fill, df_b$group)) +
  scale_y_continuous(limits = c(0, 0.9), breaks = seq(0, 0.8, 0.2)) +
  labs(y = "SPQI") +
  annotate("text", x = 0.7, y = 0.82, label = "(b)", size = 6) +
  theme_fig +
  theme(legend.position = "none")

#========================
# 7. 拼图
#========================
final_fig7 <- p_a / p_b

print(final_fig7)

#========================
# 8. 导出 PDF
#========================
ggsave("Figure7_complete.pdf", plot = final_fig7,
       width = 6, height = 8)

cat("Figure7_complete.pdf 已成功导出\n")
ggsave("Figure7_complete.png", plot = final_fig7,
       width = 6, height = 8, dpi = 600)

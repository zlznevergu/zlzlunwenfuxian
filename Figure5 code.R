#========================
# 1. 加载包
#========================
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(patchwork)) install.packages("patchwork")

library(ggplot2)
library(patchwork)

#========================
# 2. 通用设置
#========================
cols_bar <- c("0-20cm" = "#F3DDA0", "20-40cm" = "#A8DDD0")
pd <- position_dodge(0.8)

theme_bar <- theme_classic(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(color = "black"),
    axis.title.x = element_blank()
  )

theme_point <- theme_classic(base_size = 13) +
  theme(
    axis.text = element_text(color = "black"),
    legend.position = "none"
  )

#========================
# 3. (a) SAC，不同恢复类型
#========================
df_a <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(9.0, 3.8,
            13.2, 8.3,
            8.8, 11.3,
            24.0, 18.2),
  se    = c(4.0, 0.5,
            2.0, 3.2,
            1.8, 1.0,
            4.0, 2.2),
  letter = c("c", "c",
             "b", "b",
             "c", "b",
             "a", "a")
)
df_a$group <- factor(df_a$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_a$depth <- factor(df_a$depth, levels = c("0-20cm", "20-40cm"))

p_a <- ggplot(df_a, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.8, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(y = "SAC (%)") +
  annotate("text", x = 0.6, y = 28.5, label = "(a)", size = 5) +
  theme_bar

#========================
# 4. (b) Mesoporosity，不同恢复类型
#========================
df_b <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(6.4, 6.5,
            5.9, 7.3,
            5.8, 8.0,
            8.0, 7.8),
  se    = c(0.4, 0.3,
            0.4, 0.3,
            0.8, 0.7,
            0.4, 0.5),
  letter = c("b", "b",
             "b", "a",
             "b", "a",
             "a", "a")
)
df_b$group <- factor(df_b$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_b$depth <- factor(df_b$depth, levels = c("0-20cm", "20-40cm"))

p_b <- ggplot(df_b, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.4, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) +
  labs(y = "Mesoporosity (%)") +
  annotate("text", x = 0.6, y = 14.2, label = "(b)", size = 5) +
  theme_bar

#========================
# 5. (c) SPF4，不同恢复类型
# 全部蓝圈，参考线 0.287c
#========================
df_c <- data.frame(
  group = factor(c("CTF", "CTG", "CTO"), levels = c("CTO", "CTG", "CTF")),
  score = c(0.754, 0.443, 0.415)
)

p_c <- ggplot() +
  geom_segment(data = df_c,
               aes(x = 0.287, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.287, linetype = "dashed", color = "grey70") +
  geom_point(data = df_c,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 4, stroke = 1) +
  scale_x_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, 0.1)) +
  labs(x = expression(SPF[4]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.21, y = 3.15, label = "(c)", size = 5) +
  annotate("text", x = 0.30, y = 3.25, label = "0.287c", size = 4) +
  annotate("text", x = 0.77, y = 3.00, label = "0.754a", hjust = 0, size = 4) +
  annotate("text", x = 0.46, y = 2.00, label = "0.443b", hjust = 0, size = 4) +
  annotate("text", x = 0.42, y = 1.00, label = "0.415b", hjust = 0, size = 4)

#========================
# 6. (d) SAC，不同恢复年限
#========================
df_d <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(9.0, 3.8,
            9.0, 6.0,
            11.0, 5.3,
            9.2, 9.2,
            14.5, 16.4),
  se    = c(4.5, 0.5,
            0.5, 2.0,
            1.0, 1.6,
            4.0, 4.0,
            3.5, 2.5),
  letter = c("b", "c",
             "b", "bc",
             "b", "bc",
             "b", "b",
             "a", "a")
)
df_d$group <- factor(df_d$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_d$depth <- factor(df_d$depth, levels = c("0-20cm", "20-40cm"))

p_d <- ggplot(df_d, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.7, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 21), breaks = seq(0, 20, 2)) +
  labs(y = "SAC (%)") +
  annotate("text", x = 0.6, y = 19.5, label = "(d)", size = 5) +
  theme_bar

#========================
# 7. (e) Mesoporosity，不同恢复年限
#========================
df_e <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(6.4, 6.5,
            6.1, 6.2,
            6.6, 6.3,
            6.3, 7.3,
            7.0, 5.4),
  se    = c(0.5, 0.5,
            0.4, 0.4,
            0.7, 0.6,
            0.4, 0.3,
            0.6, 0.5),
  letter = c("ab", "b",
             "b", "b",
             "ab", "b",
             "ab", "a",
             "a", "c")
)
df_e$group <- factor(df_e$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_e$depth <- factor(df_e$depth, levels = c("0-20cm", "20-40cm"))

p_e <- ggplot(df_e, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.25, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(y = "Mesoporosity (%)") +
  annotate("text", x = 0.6, y = 9.5, label = "(e)", size = 5) +
  theme_bar

#========================
# 8. (f) SPF4，不同恢复年限
# 5Y 红圈下降；其余蓝圈上升；参考线 0.287bc
#========================
df_f_all <- data.frame(
  group = factor(c("35Y", "25Y", "15Y", "5Y"), levels = c("5Y", "15Y", "25Y", "35Y")),
  score = c(0.415, 0.466, 0.359, 0.255)
)

df_f_red <- data.frame(
  group = factor("5Y", levels = c("5Y", "15Y", "25Y", "35Y")),
  score = 0.255
)

df_f_blue <- data.frame(
  group = factor(c("35Y", "25Y", "15Y"), levels = c("5Y", "15Y", "25Y", "35Y")),
  score = c(0.415, 0.466, 0.359)
)

p_f <- ggplot() +
  geom_segment(data = df_f_all,
               aes(x = 0.287, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.287, linetype = "dashed", color = "grey70") +
  geom_point(data = df_f_red,
             aes(x = score, y = group),
             color = "red", shape = 21, fill = "white", size = 4, stroke = 1) +
  geom_point(data = df_f_blue,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 4, stroke = 1) +
  scale_x_continuous(limits = c(0.1, 0.7), breaks = seq(0.1, 0.7, 0.1)) +
  labs(x = expression(SPF[4]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.11, y = 4.15, label = "(f)", size = 5) +
  annotate("text", x = 0.30, y = 4.30, label = "0.287bc", size = 4) +
  annotate("text", x = 0.43, y = 4.00, label = "0.415ab", hjust = 0, size = 4) +
  annotate("text", x = 0.48, y = 3.00, label = "0.466a", hjust = 0, size = 4) +
  annotate("text", x = 0.36, y = 2.00, label = "0.359abc", hjust = 0, size = 4) +
  annotate("text", x = 0.14, y = 1.00, label = "0.255c", hjust = 0, size = 4) +
  annotate("point", x = 0.49, y = 1.55, shape = 21, size = 4, stroke = 1, color = "red", fill = "white") +
  annotate("text", x = 0.52, y = 1.55, label = "Scores decreased", hjust = 0, size = 3.2) +
  annotate("point", x = 0.49, y = 1.25, shape = 21, size = 4, stroke = 1, color = "blue", fill = "white") +
  annotate("text", x = 0.52, y = 1.25, label = "Scores increased", hjust = 0, size = 3.2)

#========================
# 9. 拼图：2×3
#========================
final_fig5 <- (p_a + p_b + p_c) / (p_d + p_e + p_f) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

print(final_fig5)

#========================
# 10. 保存 PNG 和 PDF
#========================
ggsave("Figure5_complete.png", plot = final_fig5,
       width = 14, height = 8, dpi = 600)

ggsave("Figure5_complete.pdf", plot = final_fig5,
       width = 14, height = 8)

cat("完整 Figure 5 已保存为 Figure5_complete.png 和 Figure5_complete.pdf\n")

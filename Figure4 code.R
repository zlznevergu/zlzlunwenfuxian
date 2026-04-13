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
# 3. (a) MWD，不同恢复类型
#========================
df_a <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(2.35, 2.28,
            2.95, 2.75,
            3.10, 2.85,
            4.35, 2.70),
  se    = c(0.20, 0.22,
            0.25, 0.22,
            0.30, 0.25,
            0.75, 0.25),
  letter = c("c", "b",
             "b", "ab",
             "b", "a",
             "a", "ab")
)
df_a$group <- factor(df_a$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_a$depth <- factor(df_a$depth, levels = c("0-20cm", "20-40cm"))

p_a <- ggplot(df_a, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.12, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 6.0), breaks = seq(0, 6, 1.5)) +
  labs(y = "MWD (mm)") +
  annotate("text", x = 0.6, y = 5.8, label = "(a)", size = 5) +
  theme_bar

#========================
# 4. (b) K，不同恢复类型
#========================
df_b <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(0.048, 0.042,
            0.031, 0.042,
            0.027, 0.028,
            0.021, 0.034),
  se    = c(0.012, 0.009,
            0.004, 0.008,
            0.005, 0.003,
            0.008, 0.007),
  letter = c("a", "a",
             "b", "a",
             "bc", "a",
             "c", "a")
)
df_b$group <- factor(df_b$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_b$depth <- factor(df_b$depth, levels = c("0-20cm", "20-40cm"))

p_b <- ggplot(df_b, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.003, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 0.08), breaks = seq(0, 0.07, 0.01)) +
  labs(y = expression("K (Mg h MJ"^{-1}*" mm"^{-1}*")")) +
  annotate("text", x = 0.6, y = 0.075, label = "(b)", size = 5) +
  theme_bar

#========================
# 5. (c) SPF3，不同恢复类型
# 全部蓝圈
# 参考线：0.130c
#========================
df_c <- data.frame(
  group = factor(c("CTF", "CTG", "CTO"), levels = c("CTO", "CTG", "CTF")),
  score = c(0.688, 0.574, 0.479)
)

p_c <- ggplot() +
  geom_segment(data = df_c,
               aes(x = 0.13, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.130, linetype = "dashed", color = "grey70") +
  geom_point(data = df_c,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 4, stroke = 1) +
  scale_x_continuous(limits = c(0.08, 0.72), breaks = seq(0.1, 0.7, 0.1)) +
  labs(x = expression(SPF[3]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.10, y = 3.15, label = "(c)", size = 5) +
  annotate("text", x = 0.105, y = 3.32, label = "0.130c", size = 4) +
  annotate("text", x = 0.70, y = 3.00, label = "0.688a", hjust = 0, size = 4) +
  annotate("text", x = 0.60, y = 2.00, label = "0.574ab", hjust = 0, size = 4) +
  annotate("text", x = 0.49, y = 1.00, label = "0.479b", hjust = 0, size = 4)

#========================
# 6. (d) MWD，不同恢复年限
#========================
df_d <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(2.40, 2.35,
            2.30, 1.98,
            2.55, 2.48,
            2.98, 2.55,
            3.12, 2.58),
  se    = c(0.35, 0.32,
            0.28, 0.30,
            0.30, 0.26,
            0.20, 0.40,
            0.20, 0.45),
  letter = c("b", "a",
             "b", "a",
             "b", "a",
             "a", "a",
             "a", "a")
)
df_d$group <- factor(df_d$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_d$depth <- factor(df_d$depth, levels = c("0-20cm", "20-40cm"))

p_d <- ggplot(df_d, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.10, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 4.0), breaks = seq(0, 4, 1)) +
  labs(y = "MWD (mm)") +
  annotate("text", x = 0.6, y = 3.8, label = "(d)", size = 5) +
  theme_bar

#========================
# 7. (e) K，不同恢复年限
#========================
df_e <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(0.048, 0.041,
            0.041, 0.045,
            0.040, 0.040,
            0.032, 0.042,
            0.029, 0.031),
  se    = c(0.012, 0.010,
            0.008, 0.006,
            0.007, 0.006,
            0.004, 0.009,
            0.003, 0.002),
  letter = c("a", "a",
             "ab", "a",
             "abc", "a",
             "bc", "a",
             "c", "a")
)
df_e$group <- factor(df_e$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_e$depth <- factor(df_e$depth, levels = c("0-20cm", "20-40cm"))

p_e <- ggplot(df_e, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.0025, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 0.08), breaks = seq(0, 0.07, 0.01)) +
  labs(y = expression("K (Mg h MJ"^{-1}*" mm"^{-1}*")")) +
  annotate("text", x = 0.6, y = 0.075, label = "(e)", size = 5) +
  theme_bar

#========================
# 8. (f) SPF3，不同恢复年限
# 全部蓝圈
# 参考线：0.140d
#========================
df_f <- data.frame(
  group = factor(c("35Y", "25Y", "15Y", "5Y"), levels = c("5Y", "15Y", "25Y", "35Y")),
  score = c(0.479, 0.385, 0.257, 0.144)
)

p_f <- ggplot() +
  geom_segment(data = df_f,
               aes(x = 0.14, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.140, linetype = "dashed", color = "grey70") +
  geom_point(data = df_f,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 4, stroke = 1) +
  scale_x_continuous(limits = c(0.08, 0.70), breaks = seq(0.1, 0.7, 0.1)) +
  labs(x = expression(SPF[3]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.10, y = 4.15, label = "(f)", size = 5) +
  annotate("text", x = 0.14, y = 4.30, label = "0.140d", size = 4) +
  annotate("text", x = 0.50, y = 4.00, label = "0.479a", hjust = 0, size = 4) +
  annotate("text", x = 0.40, y = 3.00, label = "0.385b", hjust = 0, size = 4) +
  annotate("text", x = 0.27, y = 2.00, label = "0.257c", hjust = 0, size = 4) +
  annotate("text", x = 0.16, y = 1.00, label = "0.144d", hjust = 0, size = 4)

#========================
# 9. 拼图：2×3
#========================
final_fig4 <- (p_a + p_b + p_c) / (p_d + p_e + p_f) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

print(final_fig4)

#========================
# 10. 保存 PNG 和 PDF
#========================
ggsave("Figure4_complete.png", plot = final_fig4,
       width = 14, height = 8, dpi = 600)

ggsave("Figure4_complete.pdf", plot = final_fig4,
       width = 14, height = 8)

cat("完整 Figure 4 已保存为 Figure4_complete.png 和 Figure4_complete.pdf\n")

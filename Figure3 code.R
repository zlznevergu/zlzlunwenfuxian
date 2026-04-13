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
# 3. (a) Ks，不同恢复类型
#========================
df_a <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(0.95, 0.32,
            1.25, 2.15,
            0.88, 0.68,
            1.72, 1.78),
  se    = c(0.70, 0.10,
            0.85, 0.45,
            0.55, 0.40,
            1.95, 1.45),
  letter = c("a", "b",
             "a", "a",
             "a", "ab",
             "a", "a")
)
df_a$group <- factor(df_a$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_a$depth <- factor(df_a$depth, levels = c("0-20cm", "20-40cm"))

p_a <- ggplot(df_a, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.12, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 4.2), breaks = seq(0, 4, 0.5)) +
  labs(y = expression("Ks (cm h"^{-1}*")")) +
  annotate("text", x = 0.6, y = 4.0, label = "(a)", size = 5) +
  theme_bar

#========================
# 4. (b) AWC，不同恢复类型
#========================
df_b <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(11.8, 12.1,
            12.0, 12.2,
            11.6, 11.4,
            10.3, 10.2),
  se    = c(0.5, 0.5,
            0.4, 0.4,
            0.5, 0.5,
            0.2, 0.2),
  letter = c("a", "a",
             "a", "a",
             "a", "b",
             "a", "c")
)
df_b$group <- factor(df_b$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_b$depth <- factor(df_b$depth, levels = c("0-20cm", "20-40cm"))

p_b <- ggplot(df_b, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.25, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 18.5), breaks = seq(0, 18, 2)) +
  labs(y = "AWC (%)") +
  annotate("text", x = 0.6, y = 18.0, label = "(b)", size = 5) +
  theme_bar

#========================
# 5. (c) SPF2，不同恢复类型
# 参考线和参考点：0.473c
# 红点：CTG 0.467c
# 蓝点：CTF 0.590b, CTO 0.668a
#========================
df_c_all <- data.frame(
  group = factor(c("CTF", "CTG", "CTO"), levels = c("CTO", "CTG", "CTF")),
  score = c(0.590, 0.467, 0.668)
)

df_c_red <- data.frame(
  group = factor("CTG", levels = c("CTO", "CTG", "CTF")),
  score = 0.467
)

df_c_blue <- data.frame(
  group = factor(c("CTF", "CTO"), levels = c("CTO", "CTG", "CTF")),
  score = c(0.590, 0.668)
)

p_c <- ggplot() +
  geom_segment(data = df_c_all,
               aes(x = 0.1, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.473, linetype = "dashed", color = "grey70") +
  geom_point(data = df_c_red,
             aes(x = score, y = group),
             color = "red", shape = 21, fill = "white", size = 4, stroke = 1) +
  geom_point(data = df_c_blue,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 4, stroke = 1) +
  geom_point(aes(x = 0.473, y = factor("CTF", levels = c("CTO", "CTG", "CTF"))),
             color = "blue", shape = 21, fill = "white", size = 4, stroke = 1) +
  scale_x_continuous(limits = c(0.1, 0.7), breaks = seq(0.1, 0.7, 0.1)) +
  labs(x = expression(SPF[2]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.12, y = 3.15, label = "(c)", size = 5) +
  annotate("point", x = 0.15, y = 2.85, shape = 21, size = 4, stroke = 1, color = "red", fill = "white") +
  annotate("text", x = 0.20, y = 2.85, label = "Scores decreased", hjust = 0, size = 3.5) +
  annotate("point", x = 0.15, y = 2.62, shape = 21, size = 4, stroke = 1, color = "blue", fill = "white") +
  annotate("text", x = 0.20, y = 2.62, label = "Scores increased", hjust = 0, size = 3.5) +
  annotate("text", x = 0.42, y = 3.34, label = "0.473c", size = 4) +
  annotate("text", x = 0.62, y = 3.02, label = "0.590b", size = 4, hjust = 0) +
  annotate("text", x = 0.49, y = 2.00, label = "0.467c", size = 4, hjust = 0) +
  annotate("text", x = 0.69, y = 1.00, label = "0.668a", size = 4, hjust = 0)

#========================
# 6. (d) Ks，不同恢复年限
#========================
df_d <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(0.95, 0.30,
            1.15, 0.68,
            1.18, 0.92,
            1.32, 1.35,
            1.25, 2.12),
  se    = c(0.72, 0.10,
            0.55, 0.30,
            0.55, 0.28,
            1.50, 0.25,
            0.85, 0.45),
  letter = c("a", "c",
             "a", "bc",
             "a", "bc",
             "a", "b",
             "a", "a")
)
df_d$group <- factor(df_d$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_d$depth <- factor(df_d$depth, levels = c("0-20cm", "20-40cm"))

p_d <- ggplot(df_d, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.12, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 3.4), breaks = seq(0, 3.0, 0.5)) +
  labs(y = expression("Ks (cm h"^{-1}*")")) +
  annotate("text", x = 0.6, y = 3.2, label = "(d)", size = 5) +
  theme_bar

#========================
# 7. (e) AWC，不同恢复年限
#========================
df_e <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(11.6, 12.1,
            10.8, 8.3,
            12.4, 12.1,
            11.9, 12.2,
            12.6, 12.1),
  se    = c(0.6, 0.4,
            0.7, 0.7,
            0.5, 0.5,
            0.3, 0.3,
            0.3, 0.3),
  letter = c("ab", "a",
             "b", "b",
             "ab", "a",
             "ab", "a",
             "a", "a")
)
df_e$group <- factor(df_e$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_e$depth <- factor(df_e$depth, levels = c("0-20cm", "20-40cm"))

p_e <- ggplot(df_e, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.22, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 18.5), breaks = seq(0, 18, 2)) +
  labs(y = "AWC (%)") +
  annotate("text", x = 0.6, y = 18.0, label = "(e)", size = 5) +
  theme_bar

#========================
# 8. (f) SPF2，不同恢复年限
# 图中4个都是蓝色增加
# 顺序从上到下：35Y 25Y 15Y 5Y
# 参考线：0.462b
#========================
df_f_all <- data.frame(
  group = factor(c("35Y", "25Y", "15Y", "5Y"), levels = c("5Y", "15Y", "25Y", "35Y")),
  score = c(0.668, 0.652, 0.601, 0.506)
)

p_f <- ggplot() +
  geom_segment(data = df_f_all,
               aes(x = 0.1, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.462, linetype = "dashed", color = "grey70") +
  geom_point(data = df_f_all,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 4, stroke = 1) +
  scale_x_continuous(limits = c(0.1, 0.7), breaks = seq(0.1, 0.7, 0.1)) +
  labs(x = expression(SPF[2]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.12, y = 4.15, label = "(f)", size = 5) +
  annotate("text", x = 0.42, y = 4.30, label = "0.462b", size = 4) +
  annotate("text", x = 0.67, y = 4.00, label = "0.668a", size = 4, hjust = 0) +
  annotate("text", x = 0.65, y = 3.00, label = "0.652a", size = 4, hjust = 0) +
  annotate("text", x = 0.60, y = 2.00, label = "0.601a", size = 4, hjust = 0) +
  annotate("text", x = 0.51, y = 1.00, label = "0.506b", size = 4, hjust = 0)

#========================
# 9. 拼图：2×3
#========================
final_fig3 <- (p_a + p_b + p_c) / (p_d + p_e + p_f) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

print(final_fig3)

#========================
# 10. 保存
#========================
ggsave("Figure3_complete.png", plot = final_fig3,
       width = 14, height = 8, dpi = 600)

cat("完整 Figure 3 已保存为 Figure3_complete.png\n")

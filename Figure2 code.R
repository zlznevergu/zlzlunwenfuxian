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
cols_point <- c("Scores decreased" = "red", "Scores increased" = "blue")
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
# 3. (a) BD by restoration type
#========================
df_a <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(1.48, 1.60, 1.36, 1.53, 1.49, 1.57, 1.35, 1.50),
  se    = c(0.05, 0.05, 0.04, 0.05, 0.05, 0.05, 0.05, 0.05),
  letter = c("a", "a", "ab", "a", "a", "a", "b", "a")
)
df_a$group <- factor(df_a$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_a$depth <- factor(df_a$depth, levels = c("0-20cm", "20-40cm"))

p_a <- ggplot(df_a, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.05, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 2.0), breaks = seq(0, 2.0, 0.5)) +
  labs(y = expression("BD (g cm"^{-3}*")")) +
  annotate("text", x = 0.6, y = 1.95, label = "(a)", size = 5) +
  theme_bar

#========================
# 4. (b) PR by restoration type
#========================
df_b <- data.frame(
  group = rep(c("CL", "CTO", "CTG", "CTF"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 4),
  mean  = c(260, 500, 540, 840, 300, 540, 340, 810),
  se    = c(120, 110, 140, 120, 110, 100, 130, 110),
  letter = c("b", "b", "a", "a", "b", "b", "b", "a")
)
df_b$group <- factor(df_b$group, levels = c("CL", "CTO", "CTG", "CTF"))
df_b$depth <- factor(df_b$depth, levels = c("0-20cm", "20-40cm"))

p_b <- ggplot(df_b, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 35, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200)) +
  labs(y = "PR (kPa)") +
  annotate("text", x = 0.6, y = 970, label = "(b)", size = 5) +
  theme_bar

#========================
# 5. (c) SPF1 scores by restoration type
#========================
df_c_seg <- data.frame(
  group = c("CTF", "CTG", "CTO"),
  score = c(0.476, 0.456, 0.280)
)
df_c_seg$group <- factor(df_c_seg$group, levels = c("CTO", "CTG", "CTF"))

df_c_blue <- data.frame(
  group = factor("CTF", levels = c("CTO", "CTG", "CTF")),
  score = 0.485
)

p_c <- ggplot() +
  geom_segment(data = df_c_seg,
               aes(x = 0.1, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.485, linetype = "dashed", color = "grey70") +
  geom_point(data = df_c_seg,
             aes(x = score, y = group),
             color = "red", shape = 21, fill = "white", size = 3.5, stroke = 1) +
  geom_point(data = df_c_blue,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 3.5, stroke = 1) +
  scale_x_continuous(limits = c(0.1, 0.7), breaks = seq(0.1, 0.7, 0.1)) +
  labs(x = expression(SPF[1]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.12, y = 3.15, label = "(c)", size = 5) +
  annotate("point", x = 0.16, y = 2.85, shape = 21, size = 3.5, stroke = 1, color = "red", fill = "white") +
  annotate("text", x = 0.22, y = 2.85, label = "Scores decreased", hjust = 0, size = 3.5) +
  annotate("point", x = 0.16, y = 2.62, shape = 21, size = 3.5, stroke = 1, color = "blue", fill = "white") +
  annotate("text", x = 0.22, y = 2.62, label = "Scores increased", hjust = 0, size = 3.5) +
  annotate("text", x = 0.49, y = 3.12, label = "0.476a", hjust = 0, size = 4) +
  annotate("text", x = 0.50, y = 2.72, label = "0.485a", hjust = 0, size = 4) +
  annotate("text", x = 0.43, y = 2.12, label = "0.456a", hjust = 0, size = 4) +
  annotate("text", x = 0.22, y = 1.12, label = "0.280b", hjust = 0, size = 4)

#========================
# 6. (d) BD by restoration age
#========================
df_d <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(1.49, 1.60, 1.35, 1.56, 1.42, 1.50, 1.48, 1.50, 1.48, 1.50),
  se    = c(0.05, 0.05, 0.04, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05),
  letter = c("a", "a", "b", "a", "ab", "a", "a", "a", "a", "a")
)
df_d$group <- factor(df_d$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_d$depth <- factor(df_d$depth, levels = c("0-20cm", "20-40cm"))

p_d <- ggplot(df_d, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 0.05, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 2.0), breaks = seq(0, 2.0, 0.5)) +
  labs(y = expression("BD (g cm"^{-3}*")")) +
  annotate("text", x = 0.6, y = 1.95, label = "(d)", size = 5) +
  theme_bar

#========================
# 7. (e) PR by restoration age
#========================
df_e <- data.frame(
  group = rep(c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"), each = 2),
  depth = rep(c("0-20cm", "20-40cm"), 5),
  mean  = c(260, 500, 250, 530, 280, 770, 540, 840, 590, 700),
  se    = c(120, 110, 100, 110, 110, 130, 140, 120, 150, 110),
  letter = c("b", "b", "b", "b", "b", "a", "a", "a", "a", "a")
)
df_e$group <- factor(df_e$group, levels = c("CL(0Y)", "5Y", "15Y", "25Y", "35Y"))
df_e$depth <- factor(df_e$depth, levels = c("0-20cm", "20-40cm"))

p_e <- ggplot(df_e, aes(x = group, y = mean, fill = depth)) +
  geom_col(width = 0.75, position = pd) +
  geom_errorbar(aes(ymin = mean, ymax = mean + se), width = 0.15, position = pd) +
  geom_text(aes(y = mean + se + 35, label = letter), position = pd, size = 4.5) +
  scale_fill_manual(values = cols_bar) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200)) +
  labs(y = "PR (kPa)") +
  annotate("text", x = 0.6, y = 970, label = "(e)", size = 5) +
  theme_bar

#========================
# 8. (f) SPF1 scores by restoration age
#========================
df_f_red <- data.frame(
  group = factor(c("35Y", "25Y"), levels = c("5Y", "15Y", "25Y", "35Y")),
  score = c(0.280, 0.289)
)

df_f_blue <- data.frame(
  group = factor(c("15Y", "5Y"), levels = c("5Y", "15Y", "25Y", "35Y")),
  score = c(0.497, 0.655)
)

df_f_all <- data.frame(
  group = factor(c("35Y", "25Y", "15Y", "5Y"), levels = c("5Y", "15Y", "25Y", "35Y")),
  score = c(0.280, 0.289, 0.497, 0.655)
)

p_f <- ggplot() +
  geom_segment(data = df_f_all,
               aes(x = 0.1, xend = score, y = group, yend = group),
               color = "grey70", linewidth = 0.8) +
  geom_vline(xintercept = 0.476, linetype = "dashed", color = "grey70") +
  geom_point(data = df_f_red,
             aes(x = score, y = group),
             color = "red", shape = 21, fill = "white", size = 3.5, stroke = 1) +
  geom_point(data = df_f_blue,
             aes(x = score, y = group),
             color = "blue", shape = 21, fill = "white", size = 3.5, stroke = 1) +
  scale_x_continuous(limits = c(0.1, 0.7), breaks = seq(0.1, 0.7, 0.1)) +
  labs(x = expression(SPF[1]~scores), y = NULL) +
  theme_point +
  annotate("text", x = 0.12, y = 4.15, label = "(f)", size = 5) +
  annotate("text", x = 0.22, y = 4.12, label = "0.280b", hjust = 0, size = 4) +
  annotate("text", x = 0.23, y = 3.12, label = "0.289b", hjust = 0, size = 4) +
  annotate("text", x = 0.43, y = 2.12, label = "0.497ab", hjust = 0, size = 4) +
  annotate("text", x = 0.62, y = 1.12, label = "0.655a", hjust = 0, size = 4) +
  annotate("text", x = 0.40, y = 3.75, label = "0.476ab", hjust = 0, size = 4)

#========================
# 9. 拼图
#========================
final_fig2 <- (p_a + p_b + p_c) / (p_d + p_e + p_f) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

print(final_fig2)

#========================
# 10. 保存
#========================
ggsave("Figure2_final_fixed.png", plot = final_fig2,
       width = 14, height = 8, dpi = 600)

cat("完整 Figure 2 已保存为 Figure2_final_fixed.png\n")
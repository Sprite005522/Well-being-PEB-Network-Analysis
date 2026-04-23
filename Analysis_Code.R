# =============================================================
# Project: Psychological Well-being and Pro-environmental Behavior
# Methods: GGM, DAG, NCT, Stability Analysis
# Author: Fuhua Yang (Co-first Author),Ye Han (Co-first Author),Dejun Cheng (Co-first Author), Jiaci Lin (Corresponding Author)
# Contact: jiacilin0210@smail.nju.edu.cn
# Date: 2026-04
# =============================================================


# ==============================================================================
# Research on Pro-environmental Behavior (PEB) and Well-being (WB)
# ==============================================================================

# -------------------------------------
# 1. Environment Setup and Package Loading
# -------------------------------------
if (!require("qgraph")) install.packages("qgraph"); library(qgraph)
if (!require("bootnet")) install.packages("bootnet"); library(bootnet)
if (!require("mgm")) install.packages("mgm"); library(mgm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("networktools")) install.packages("networktools"); library(networktools)
if (!require("NetworkComparisonTest")) install.packages("NetworkComparisonTest"); library(NetworkComparisonTest)
if (!require("bnlearn")) install.packages("bnlearn"); library(bnlearn)
if (!require("tidyverse")) library(tidyverse)
if (!require("igraph")) library(igraph)
if (!require("psych")) library(psych)
if (!require("flextable")) install.packages("flextable"); library(flextable)
if (!require("officer")) install.packages("officer"); library(officer) 

# -------------------------------------
# 2. Data Loading and Preprocessing (Variable Alignment)
# -------------------------------------
# Ensure the dataset "data_wellbeing_peb_clean.csv" is in the working directory
data_raw <- read.csv("data_wellbeing_peb_clean.csv", stringsAsFactors = FALSE)

# Study variables: CB, EC, SFC, Tra, EWB, SWB, PWB
data <- data.frame(
  CB  = rowMeans(data_raw[, grep("PEBS_Conservation", colnames(data_raw))], na.rm = TRUE),
  EC  = rowMeans(data_raw[, grep("PEBS_Environmental_citizenship", colnames(data_raw))], na.rm = TRUE),
  SFC = rowMeans(data_raw[, grep("PEBS_Food", colnames(data_raw))], na.rm = TRUE),
  Tra = rowMeans(data_raw[, grep("PEBS_Tmnsporation", colnames(data_raw))], na.rm = TRUE),  
  EWB = rowMeans(data_raw[, grep("Mental_Health_Continuum_EWB", colnames(data_raw))], na.rm = TRUE),
  SWB = rowMeans(data_raw[, grep("Mental_Health_Continuum_SWB", colnames(data_raw))], na.rm = TRUE),
  PWB = rowMeans(data_raw[, grep("Mental_Health_Continuum_PWB", colnames(data_raw))], na.rm = TRUE)
)
data <- na.omit(data)
data <- as.data.frame(lapply(data, as.numeric))

# Plotting constant settings
longnames <- c("Conservation behavior", "Environmental citizenship", "Sustainable food consumption", 
               "Sustainable transportation", "Emotional Well-being", "Social Well-being", "Psychological Well-being")
comms <- c(rep("Pro-environmental Behavior", 4), rep("Well-being", 3))
colors <- c("#bdd7e7", "#c7e9c0") 
col_list <- list("PEB" = 1:4, "WB" = 5:7)

# ==============================================================================
# Part 1: Main Figures
# ==============================================================================

# -------------------------------------
# 3. Figure 1: Global Network Analysis
# -------------------------------------

# Fig 1a: GGM Network Plot
set.seed(123)
NET <- estimateNetwork(data, default = "EBICglasso", lambda.min.ratio = 0.001)
fit_mgm <- mgm(as.matrix(data), type = rep("g", 7), level = rep(1, 7), lambdaSel = "EBIC")
pred <- predict(fit_mgm, data, errorCon = "R2")

# Maintain layout consistency
L <- qgraph(NET$graph, layout = "spring", groups = col_list, DoNotPlot = TRUE)$layout

plot(NET, layout = L, groups = col_list, nodeNames = longnames, color = colors,
     pie = pred$error$R2, pieColor = c(rep(colors[1], 4), rep(colors[2], 3)),
     negDashed = TRUE, borders = TRUE, legend = TRUE, 
     main = "Figure 1a: GGM Network", legend.cex = 0.45)



# Fig 1b: Community Detection Heatmap (Replicating 4 algorithms)
adj_abs <- abs(NET$graph)
g_abs <- graph_from_adjacency_matrix(adj_abs, mode = "undirected", weighted = TRUE, diag = FALSE)
set.seed(2024)
T_Community <- data.frame(
  "Walktrap"    = as.factor(membership(cluster_walktrap(g_abs))),
  "Fast_Greedy" = as.factor(membership(cluster_fast_greedy(g_abs))),
  "Leiden"      = as.factor(membership(cluster_leiden(g_abs, objective_function="modularity"))),
  "Louvain"     = as.factor(apply(replicate(100, membership(cluster_louvain(g_abs))), 1, function(x) names(which.max(table(x)))))
)
community_long <- T_Community %>% rownames_to_column("Node") %>% 
  pivot_longer(cols = -Node, names_to = "Algorithm", values_to = "Community")

p_1b <- ggplot(community_long, aes(x = factor(Algorithm, levels = c("Walktrap", "Fast_Greedy", "Leiden", "Louvain")), 
                                   y = factor(Node, levels = colnames(data)), fill = Community)) +
  geom_tile(color = "white", linewidth = 1.2) +
  scale_fill_manual(values = c("1" = "#CAB2D6", "2" = "#A6CEE3", "3" = "#FDBF6F")) +
  geom_text(aes(label = Community), color = "white", fontface = "bold", size = 5) +
  theme_minimal() + labs(title = "Figure 1b: Community Detection Comparison", x = "Algorithm", y = "Node") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
print(p_1b)


# -------------------------------------------------------
# Figure 1c: Centrality Indicators (Replicating logic and style)
# -------------------------------------------------------

# 1. Expected Influence (EI)
centralityPlot(NET, theme_bw = TRUE, scale = "z-scores", include = "ExpectedInfluence", decreasing = TRUE) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  theme(axis.text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  ggtitle("Figure 1c: Expected Influence", subtitle = "Pro-environmental Behavior & Well-being")

# 2. Bridge Expected Influence (BEI)
bridge_centrality <- bridge(NET$graph, communities = comms, normalize = FALSE)
plot(bridge_centrality, include = "Bridge Expected Influence (1-step)", order = "alphabetical", zscore = TRUE) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  theme_bw() + 
  theme(axis.text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  ggtitle("Figure 1c: Bridge Expected Influence", subtitle = "Pro-environmental Behavior & Well-being")



# ==============================================================================
# 4. Figure 2: Internal Split-sample Replication 
# ==============================================================================

set.seed(20240904)
df_replication <- data[, 1:7]

# 4.0 Stratified Random Split-half based on original source logic
if ("性别" %in% names(data)) {
  idx_w <- which(data$性别 == "女")
  idx_m <- which(data$性别 == "男")
  A_w   <- sample(idx_w, length(idx_w) %/% 2)
  A_m   <- sample(idx_m, length(idx_m) %/% 2)
  A_idx <- sort(c(A_w, A_m))
} else {
  A_idx <- sample(seq_len(nrow(df_replication)), nrow(df_replication) %/% 2)
}
B_idx <- setdiff(seq_len(nrow(df_replication)), A_idx)
df_A <- df_replication[A_idx, ]
df_B <- df_replication[B_idx, ]

# 4.1 Estimating networks and predictability for split samples
NET_A <- estimateNetwork(df_A, default = "EBICglasso", lambda.min.ratio = 0.001)
NET_B <- estimateNetwork(df_B, default = "EBICglasso", lambda.min.ratio = 0.001)

fit_A <- mgm(as.matrix(df_A), type = rep("g", 7), level = rep(1,7), lambdaSel = "EBIC")
fit_B <- mgm(as.matrix(df_B), type = rep("g", 7), level = rep(1,7), lambdaSel = "EBIC")

pred_A <- predict(fit_A, df_A, errorCon = "R2")
pred_B <- predict(fit_B, df_B, errorCon = "R2")

# 4.1 Estimating networks and predictability for split samples
avg_layout_rep <- averageLayout(list(NET_A$graph, NET_B$graph))

# Generate edge color matrix
edge_col_A <- matrix("black", 7, 7); edge_col_B <- matrix("black", 7, 7)
edge_col_A[NET_A$graph > 0] <- "blue"; edge_col_A[NET_A$graph < 0] <- "red"
edge_col_B[NET_B$graph > 0] <- "blue"; edge_col_B[NET_B$graph < 0] <- "red"
node_pie_colors_rep <- c(rep(colors[1], 4), rep(colors[2], 3))

# Figure 2a: Split-sample Network Comparison
par(mfrow = c(1,2))
qgraph(NET_A$graph, layout = avg_layout_rep, groups = col_list, nodeNames = longnames, color = colors,
       pie = pred_A$error$R2, pieColor = node_pie_colors_rep,
       negDashed = TRUE, borders = TRUE, legend = FALSE, threshold = 0, label.cex = 1.2,
       edge.color = edge_col_A, main = paste0("Replication Sample A (n = ", nrow(df_A), ")"))

qgraph(NET_B$graph, layout = avg_layout_rep, groups = col_list, nodeNames = longnames, color = colors,
       pie = pred_B$error$R2, pieColor = node_pie_colors_rep,
       negDashed = TRUE, borders = TRUE, legend = FALSE, threshold = 0, label.cex = 1.2,
       edge.color = edge_col_B, main = paste0("Replication Sample B (n = ", nrow(df_B), ")"))
par(mfrow = c(1,1))


# ==============================================================================
# Figure 2b: BEI Stability Comparison
# ==============================================================================
library(tidyverse)
library(gridExtra)

# 1. Extract 1-step BEI
nodes_vec <- c("CB", "EC", "SFC", "Tra", "EWB", "SWB", "PWB")
bc_A <- bridge(NET_A$graph, communities = comms, normalize = FALSE)
bc_B <- bridge(NET_B$graph, communities = comms, normalize = FALSE)

plot_df_base <- tibble(
  Node = factor(nodes_vec, levels = nodes_vec),
  BEI_A_raw = as.numeric(bc_A$`Bridge Expected Influence (1-step)`),
  BEI_B_raw = as.numeric(bc_B$`Bridge Expected Influence (1-step)`),
  BEI_A_z = as.numeric(scale(BEI_A_raw)),
  BEI_B_z = as.numeric(scale(BEI_B_raw))
)

# 2. Identify maximum difference node
plot_diffs <- plot_df_base %>% mutate(diff_abs = abs(BEI_A_raw - BEI_B_raw))
max_row <- plot_diffs[which.max(plot_diffs$diff_abs), ]
max_node <- as.character(max_row$Node)
max_A <- round(max_row$BEI_A_raw, 3)
max_B <- round(max_row$BEI_B_raw, 3)
max_delta <- round(max_row$diff_abs, 3)

# 3. Styling definitions
custom_colors <- c("Sample A" = "#F8766D", "Sample B" = "#00BFC4")
custom_shapes <- c("Sample A" = 16, "Sample B" = 17) # 实心圆与三角

# 4. Plot A: Replication BEI comparison (raw)
p_raw <- plot_df_base %>%
  select(Node, BEI_A_raw, BEI_B_raw) %>%
  pivot_longer(cols = -Node, names_to = "Sample", values_to = "BEI") %>%
  mutate(Sample = ifelse(grepl("_A", Sample), "Sample A", "Sample B")) %>%
  ggplot(aes(x = Node, y = BEI, group = Sample, color = Sample)) +
  
  geom_line(aes(linetype = Sample), size = 1.1) + 
  geom_point(aes(shape = Sample), size = 3.5) +

  geom_vline(xintercept = which(levels(plot_df_base$Node) == max_node), linetype = "dotted", color = "grey50") +
  
  annotate("text", x = max_node, y = max(max_A, max_B) + 0.02, 
           label = sprintf("A=%.3f B=%.3f Δ=%.3f", max_A, max_B, max_delta),
           color = "black", vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_y_continuous(limits = c(0, max(plot_df_base$BEI_A_raw, plot_df_base$BEI_B_raw) + 0.05)) +
  labs(title = "Replication BEI comparison (raw)", x = NULL, y = "Bridge Expected Influence (1-step)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "top",
        panel.grid.minor = element_blank())

# 5. Plot B: Replication BEI comparison (z-scored)
p_z <- plot_df_base %>%
  select(Node, BEI_A_z, BEI_B_z) %>%
  pivot_longer(cols = -Node, names_to = "Sample", values_to = "BEI_z") %>%
  mutate(Sample = ifelse(grepl("_A", Sample), "Sample A", "Sample B")) %>%
  ggplot(aes(x = Node, y = BEI_z, group = Sample, color = Sample)) +
  geom_line(aes(linetype = Sample), size = 1.1) + 
  geom_point(aes(shape = Sample), size = 3.5) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  scale_y_continuous(limits = c(-2.5, 2.5)) + # 严格固定 z 分数区间使图表匀称
  labs(title = "Replication BEI comparison (z-scored)", x = NULL, y = "BEI (z-score)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "top",
        panel.grid.minor = element_blank())


grid.arrange(p_raw, p_z, nrow = 1)


# ==============================================================================
# 6. Figure 3: Gender Difference Network Analysis
# ==============================================================================

# 6.0 Data Alignment (FIXED: Join gender BEFORE removing missing values)
# Create a temporary dataframe that includes Gender to ensure rows stay aligned
data_temp <- data.frame(
  CB  = rowMeans(data_raw[, grep("PEBS_Conservation", colnames(data_raw))], na.rm = TRUE),
  EC  = rowMeans(data_raw[, grep("PEBS_Environmental_citizenship", colnames(data_raw))], na.rm = TRUE),
  SFC = rowMeans(data_raw[, grep("PEBS_Food", colnames(data_raw))], na.rm = TRUE),
  Tra = rowMeans(data_raw[, grep("PEBS_Tmnsporation", colnames(data_raw))], na.rm = TRUE),  
  EWB = rowMeans(data_raw[, grep("Mental_Health_Continuum_EWB", colnames(data_raw))], na.rm = TRUE),
  SWB = rowMeans(data_raw[, grep("Mental_Health_Continuum_SWB", colnames(data_raw))], na.rm = TRUE),
  PWB = rowMeans(data_raw[, grep("Mental_Health_Continuum_PWB", colnames(data_raw))], na.rm = TRUE),
  Gender = data_raw$Gender # <--- Join here
)

# Now remove rows with missing values - Gender stays with the correct row!
data_clean <- na.omit(data_temp)

# Split samples using the correctly aligned Gender column
df_m <- data_clean[data_clean$Gender == 1, 1:7] # Male
df_f <- data_clean[data_clean$Gender == 2, 1:7] # Female

cat("Correct Sample Sizes: Men =", nrow(df_m), ", Women =", nrow(df_f), "\n")

# 6.1 Estimating gender-specific networks
NET_m <- estimateNetwork(df_m, default = "EBICglasso", lambda.min.ratio = 0.001)
NET_f <- estimateNetwork(df_f, default = "EBICglasso", lambda.min.ratio = 0.001)

fit_m <- mgm(as.matrix(df_m), type = rep("g", 7), level = rep(1, 7), lambdaSel = "EBIC")
fit_f <- mgm(as.matrix(df_f), type = rep("g", 7), level = rep(1, 7), lambdaSel = "EBIC")

pred_m <- predict(fit_m, df_m, errorCon = "R2")
pred_f <- predict(fit_f, df_f, errorCon = "R2")

avg_layout_gender <- averageLayout(list(NET_m$graph, NET_f$graph))

# ------------------------------------------------------------------------------
# Figure 3c & 3d: Network Comparison Test (NCT)
# ------------------------------------------------------------------------------


library(NetworkComparisonTest)

# 1. Execute NCT Calculation
# We use 1,000 permutations to estimate the null distribution for the test statistics. [cite: 94, 95]
set.seed(123)
nct_gender <- NCT(
  data1 = df_f,                
  data2 = df_m,                
  it = 1000,                   
  binary.data = FALSE,         
  test.edges = FALSE,          
  progressbar = TRUE
)

# 2. Statistical Summary
# Review the observed M (Structure Invariance) and S (Global Strength) values along with p-values. [cite: 140]
summary(nct_gender)

# 3. Figure 3c: Global Strength Difference Distribution
# Visualizes the permutation distribution of the difference in overall connectivity (Global Strength). [cite: 140, 402]
plot(nct_gender, 
     what = "strength", 
     main = "Figure 3c: Global Strength Difference by Gender")

# 4. Figure 3d: Network Structure Invariance Test
# Visualizes the permutation distribution of the maximum edge weight difference (M). [cite: 402, 403]
plot(nct_gender, 
     what = "network", 
     main = "Figure 3d: Network Structure Difference by Gender")



# ==============================================================================
# Step 6: Figure 4 - Directed Acyclic Graph (DAG) Analysis
# ==============================================================================

node_bg_colors <- c(
  rep("#c7e9c0", 4),  # 第一组节点（Pro-environmental Behavior）的背景色
  rep("#bdd7e7", 3)   # 第二组节点（Well-being）的背景色
)

netdata <- data %>%
  rename(
    CB  = any_of(c("CB", "Con")),
    EC  = any_of(c("EC", "Env")),
    SFC = any_of(c("SFC", "Food")),
    Tra = any_of("Tra"),
    EWB = any_of("EWB"),
    SWB = any_of("SWB"),
    PWB = any_of("PWB")
  ) %>%
  select(CB, EC, SFC, Tra, EWB, SWB, PWB)


if (require("bnlearn") && require("bnviewer")) {
  
  
  set.seed(2023) 
  bootnet_bn <- boot.strength(
    netdata, 
    R = 5000, 
    algorithm = "hc",
    algorithm.args = list(restart = 50, perturb = 100)
  )
  
  avgnet <- averaged.network(bootnet_bn)
  thresh <- avgnet$learning$args[[1]]
  
  astr2 <- bootnet_bn[bootnet_bn$strength > thresh & bootnet_bn$direction > 0.50, ]
  astr2$strength <- astr2$direction
  
  
  clusters <- list(
    list(
      label = "Pro-environmental Behavior", 
      shape = "icon", 
      icon = list(code = "f135", color = "#c7e9c0"), 
      nodes = colnames(netdata)[1:4]
    ),
    list(
      label = "Well-being", 
      shape = "icon", 
      icon = list(code = "f0e7", color = "#bdd7e7"), 
      nodes = colnames(netdata)[5:7]
    )
  )
  
  
  fig4a <- strength.viewer(
    avgnet,
    bootnet_bn,
    bayesianNetwork.width = "100%",
    bayesianNetwork.height = "600px",
    bayesianNetwork.layout = "layout_components", 
    node.size = 6,
    node.shape = "ellipse",
    node.font = list(color = "black", face = "Arial"),
    node.colors = list(
      background = node_bg_colors, 
      border = "black", 
      highlight = list(background = "#e91eba", border = "black")
    ),
    edges.smooth = TRUE,
    edges.dashes = FALSE,
    clusters = clusters
  )
  
  # Figure 4b: Filtered Network [cite: 188]
  fig4b <- strength.viewer(
    avgnet,
    astr2,
    bayesianNetwork.width = "100%",
    bayesianNetwork.height = "600px",
    bayesianNetwork.layout = "layout_components",
    node.size = 6,
    node.shape = "ellipse",
    node.font = list(color = "black", face = "Arial"),
    node.colors = list(
      background = node_bg_colors, 
      border = "black",
      highlight = list(background = "#e91eba", border = "black")
    ),
    edges.smooth = TRUE,
    edges.dashes = FALSE,
    clusters = clusters
  )
  
} else {
  message("The bnlearn/bnviewer packages are missing, so the Bayesian network cannot be plotted.")
}

# Figure 4a & 4b (Bayesian Network visualization)
print(fig4a)
print(fig4b)







# ==============================================================================
# Supplementary Material - Table S1: Descriptive Statistics
# ==============================================================================

analysis_data <- data %>%
  mutate(Age = data_raw$Age[as.numeric(rownames(data))]) %>%
  rename(
    CB  = any_of(c("CB", "Con")),   
    EC  = any_of(c("EC", "Env")),   
    SFC = any_of(c("SFC", "Food")), 
    Tra = any_of("Tra"),
    EWB = any_of("EWB"),
    SWB = any_of("SWB"),
    PWB = any_of("PWB")
  ) %>%
  select(Age, CB, EC, SFC, Tra, EWB, SWB, PWB) 


get_msd_row <- function(var_name, label, group_name) {
  if(!var_name %in% names(analysis_data)) {
    stop(paste("错误：在数据框中找不到变量", var_name))
  }
  
  total_vec <- analysis_data[[var_name]]
  
   
  m_val  <- sprintf("%.2f", mean(total_vec, na.rm=TRUE))
  sd_val <- sprintf("%.2f", sd(total_vec, na.rm=TRUE))
  
  return(data.frame(
    Category = group_name,
    Variable = label,
    M = m_val,
    SD = sd_val,
    stringsAsFactors = FALSE
  ))
}


row1 <- get_msd_row("Age", "Age (years)", "Demographics")
row2 <- get_msd_row("CB",  "Conservation behavior (CB)",        "Pro-environmental Behaviors")
row3 <- get_msd_row("EC",  "Environmental citizenship (EC)",    "Pro-environmental Behaviors")
row4 <- get_msd_row("SFC", "Sustainable food consumption (SFC)","Pro-environmental Behaviors")
row5 <- get_msd_row("Tra", "Sustainable transportation (Tra)",  "Pro-environmental Behaviors")
row6 <- get_msd_row("EWB", "Emotional well-being (EWB)",      "Well-beings")
row7 <- get_msd_row("SWB", "Social well-being (SWB)",         "Well-beings")
row8 <- get_msd_row("PWB", "Psychological well-being (PWB)",  "Well-beings")

 
table1_df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8)


ft_s1 <- flextable(table1_df) %>%
  set_header_labels(
    Category = "Category", Variable = "Variable", M = "M", SD = "SD"
  ) %>%
  merge_v(j = "Category") %>%
  valign(j = "Category", valign = "top") %>%
  align(j = 3:4, align = "center", part = "all") %>% 
  autofit() %>%
  border_remove() %>%
  hline_top(part="header", border = fp_border(color="black", width = 1.5)) %>%
  hline_bottom(part="header", border = fp_border(color="black", width = 1.0)) %>%
  hline_bottom(part="body", border = fp_border(color="black", width = 1.5)) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10.5, part = "all") %>%
  set_caption(paste0("Table S1. Descriptive statistics of the study variables (N = ", nrow(analysis_data), ")")) 


print(ft_s1)










# ==============================================================================
# Table S2: Correlation Matrix, Distribution, and Reliability
# ==============================================================================

library(Hmisc)
library(psych)
library(dplyr)


standard_data <- data %>%
  rename(
    CB  = any_of(c("CB", "Con")),
    EC  = any_of(c("EC", "Env")),
    SFC = any_of(c("SFC", "Food")),
    Tra = any_of("Tra"),
    EWB = any_of("EWB"),
    SWB = any_of("SWB"),
    PWB = any_of("PWB")
  )


cor_nodes <- c("CB", "EC", "SFC", "Tra", "EWB", "SWB", "PWB")
cor_res <- rcorr(as.matrix(standard_data[, cor_nodes]))
cor_mat <- cor_res$r
p_mat <- cor_res$P


cor_formatted <- matrix("", nrow = 7, ncol = 7)
for (i in 1:7) {
  for (j in 1:7) {
    if (j < i) {
      stars <- ifelse(p_mat[i,j] < 0.001, "***", ifelse(p_mat[i,j] < 0.01, "**", ifelse(p_mat[i,j] < 0.05, "*", "")))
      cor_formatted[i,j] <- paste0(sprintf("%.2f", cor_mat[i,j]), stars)
    } else if (i == j) {
      cor_formatted[i,j] <- "-"
    }
  }
}


dimensions_s2 <- c("PEBS_Conservation", "PEBS_Environmental_citizenship", "PEBS_Food", "PEBS_Tmnsporation",
                   "Mental_Health_Continuum_EWB", "Mental_Health_Continuum_SWB", "Mental_Health_Continuum_PWB")
alpha_vals <- c()
for (dim in dimensions_s2) {
  items <- data_raw[as.numeric(rownames(data)), grep(dim, colnames(data_raw)), drop = FALSE]
  alpha_vals <- c(alpha_vals, round(psych::alpha(na.omit(items), check.keys=TRUE)$total$raw_alpha, 2))
}


s2_stats <- psych::describe(standard_data[, cor_nodes])
means <- sprintf("%.2f", s2_stats$mean)
sds   <- sprintf("%.2f", s2_stats$sd)
skews <- sprintf("%.2f", s2_stats$skew)
kurts <- sprintf("%.2f", s2_stats$kurtosis)

ranges <- c("1-5", "1-4.67", "1-5", "1-5", "0-5", "0-5", "0-5")


table_s2_df <- data.frame(
  "Study variables" = c("1. CB", "2. EC", "3. SFC", "4. Tra", "5. EWB", "6. SWB", "7. PWB"),
  as.data.frame(cor_formatted),
  check.names = FALSE
)
colnames(table_s2_df)[2:8] <- 1:7


extra_rows <- data.frame(
  "Study variables" = c("Mean", "SD", "Range", "Skewness", "Kurtosis", "α"),
  matrix(rbind(means, sds, ranges, skews, kurts, alpha_vals), nrow = 6)
)
colnames(extra_rows) <- colnames(table_s2_df)
table_s2_final <- rbind(table_s2_df, extra_rows)

library(flextable)
library(officer)
ft_s2 <- flextable(table_s2_final) %>%
  set_caption("Table S2. Means, standard deviations, Cronbach's alpha coefficients, and the zero-order Pearson correlations.") %>%
  theme_booktabs() %>%
  align(j = 2:8, align = "center", part = "all") %>%
  autofit() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  hline(i = 7, border = fp_border(color="black", width = 1)) %>% 
  add_footer_lines("Note. * p < 0.05, ** p < 0.01, *** p < 0.001.")

print(ft_s2)










# ==============================================================================
# Table S3: Centrality Comparison between Samples
# ==============================================================================
library(networktools)


ei_orig   <- rowSums(NET$graph) 
bc_orig   <- bridge(NET$graph, communities = comms, normalize = FALSE)$`Bridge Expected Influence (1-step)`
pred_orig <- pred$error$R2


ei_rep   <- rowSums(NET_A$graph)
bc_rep   <- bridge(NET_A$graph, communities = comms, normalize = FALSE)$`Bridge Expected Influence (1-step)`
pred_rep <- pred_A$error$R2


table_s3_final <- data.frame(
  Node = c("CB", "EC", "SFC", "Tra", "EWB", "SWB", "PWB"),
  EI_Original = round(as.numeric(ei_orig), 3),
  BEI_Original = round(as.numeric(bc_orig), 3),
  Predictability_Original = round(as.numeric(pred_orig), 3),
  EI_Replication = round(as.numeric(ei_rep), 3),
  BEI_Replication = round(as.numeric(bc_rep), 3),
  Predictability_Replication = round(as.numeric(pred_rep), 3)
)


ft_s3 <- flextable(table_s3_final) %>%
  set_header_df(
    mapping = data.frame(
      col_keys = colnames(table_s3_final),
      line1 = c("Node", rep("Original sample", 3), rep("Replication sample", 3)),
      line2 = c("", "EI", "BEI", "Predictability", "EI", "BEI", "Predictability")
    ),
    key = "col_keys"
  ) %>%
  merge_h(part = "header") %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  set_caption("Table S3. Raw centrality, bridge centrality, and predictability of nodes in the GGMs of original and replication samples.")


print(ft_s3)










# ==============================================================================
# Table S4: DAG Path Significance (Importance & Robustness)
# ==============================================================================
library(bnlearn)
library(dplyr)
library(flextable)


dag_node_names <- nodes(avgnet)


strength_input_data <- netdata
colnames(strength_input_data) <- dag_node_names


bic_values <- arc.strength(avgnet, data = strength_input_data, criterion = "bic-g")


dir_probs <- bootnet_bn[, c("from", "to", "direction")]


table_s4_raw <- data.frame(arcs(avgnet)) %>%
  left_join(bic_values, by = c("from", "to")) %>%
  left_join(dir_probabilities, by = c("from", "to"))


name_mapping <- c(Con="CB", Env="EC", Food="SFC", Tra="Tra", EWB="EWB", SWB="SWB", PWB="PWB")

table_s4_final <- table_s4_raw %>%
  mutate(
    From = name_mapping[from],
    To   = name_mapping[to]
  ) %>%
  select(From, To, BIC = strength, Prob = direction) %>%
  arrange(BIC) 


ft_s4 <- flextable(table_s4_final) %>%
  set_header_labels(
    From = "From Node", To = "To Node",
    BIC = "Importance in Fig. 4a (ΔBIC)",
    Prob = "Robustness in Fig. 4b (Directional Probability)"
  ) %>%
  theme_booktabs() %>%
  colformat_double(j = 3:4, digits = 2) %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  set_caption("Table S4. Directional Probabilities and BIC Values for the 7-Variable DAG")

print(ft_s4)














# ==============================================================================
# Supplementary Figures: Robustness Analysis (Fig. S1, S2, S3)
# ==============================================================================
# 1. Non-parametric Bootstrap (S1 & S2)
# 2. Case-dropping Bootstrap (S3)

set.seed(123) 
boot_edges <- bootnet(NET, 
                      nBoots = 1000, 
                      type = "nonparametric", 
                      statistics = "edge", 
                      nCores = parallel::detectCores() - 1)

# --- Fig. S1: 95% Confidence Interval Plot of Edge Weights  ---
plot(boot_edges, labels = FALSE, order = "sample") + 
  theme_bw() + 
  ggtitle("Fig. S1: Bootstrapped 95% CI of Edge Weights")

# --- Fig. S2: Confidence Intervals for Expected Influence   ---
boot_ei <- bootnet(NET, type = "nonparametric", statistics = "expectedInfluence", 
                   nBoots = 1000, nCores = 1)
plot(boot_ei, statistics = "expectedInfluence", labels = TRUE, order = "sample",
     main = "Fig. S2: Bootstrapped 95% CI of Expected Influence", graph = "ridges",
     nodeNames = longnames, color = colors[match(comms, c("Pro-environmental Behavior", "Well-being"))])



set.seed(123)
boot_stability <- bootnet(NET, 
                          nBoots = 1000, 
                          type = "case", 
                          statistics = c("expectedInfluence", "bridgeExpectedInfluence"), 
                          communities = comms, 
                          nCores = parallel::detectCores() - 1)

# --- Fig. S3: Centrality Stability Estimates ---
plot(boot_stability, statistics = c("expectedInfluence", "bridgeExpectedInfluence")) + 
  theme_bw() + 
  theme(legend.position = "top") +
  ggtitle("Fig. S3: Stability estimations of BEI and EI")

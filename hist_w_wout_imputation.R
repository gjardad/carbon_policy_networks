# Define RGB colors
blue_rgb <- rgb(0, 114, 178, maxColorValue = 255)
orange_rgb <- rgb(255, 127, 0, maxColorValue = 255)

# Data with descriptive fill labels
df <- data.frame(
  Imputation = rep(c("W/out imputation", "With imputation"), each = 2),
  Segment = rep(c("Reallocation", "Abatement"), 2),
  Value = c(0.32, 0.05, 0.2, 0.05)
)


ggplot(df, aes(x = Imputation, y = Value, fill = Segment)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(
    values = c(
      "Reallocation" = blue_rgb,
      "Abatement" = orange_rgb
    ),
    name = NULL  # Optional: remove the title of the legend
  ) +
  geom_text(
    aes(label = Value), 
    position = position_stack(vjust = 0.5), 
    color = "white", size = 5
  ) +
  labs(y = "Change in emissions (%)", x = "") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.text = element_text(size = 14),
    legend.position = "right"
  )


ggplot(df, aes(x = Imputation, y = Value, fill = Segment)) +
  geom_bar(stat = "identity", width = 0.5, position = "stack") +
  scale_fill_manual(
    values = c("Reallocation" = blue_rgb, "Abatement" = orange_rgb),
    name = NULL
  ) +
  geom_text(
    aes(label = Value),
    position = position_stack(vjust = 0.5),
    color = "white", size = 5
  ) +
  labs(y = "Change in emissions (%)", x = "") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.text = element_text(size = 14),
    legend.position = "bottom"  # ??? Legend below the plot
  )

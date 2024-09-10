# Load required packages
pacman::p_load(ggplot2)

# Generate data
x <- seq(5, 20, 0.05)
y <- ifelse(x >= 13.85, x - 13.85, 0)

# Create plot
ggplot(data.frame(x, y), aes(x, y)) +
  geom_vline(xintercept = 13.85, linetype = "dotted") +
  geom_line() +
  theme_bw() +
  labs(
    x = "Rent prior to rent cap policy\n(Euro per sqm)",
    y = "Rent reduction\n(Euro per sqm)"
  ) +
  scale_x_continuous(breaks = c(5, 10, 13.85, 15, 20))

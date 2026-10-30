install.packages("palmerpenguins")   # une seule fois
library(palmerpenguins)
data(penguins)
head(penguins)                       # comme un SELECT * LIMIT 6

# EXO 1

nrow(penguins[penguins$island == "Biscoe", ])

gentoo_penguins <- penguins[penguins$species == "Gentoo", ]

mean(gentoo_penguins$body_mass_g, na.rm = TRUE)

sum(penguins$body_mass_g > 4000, na.rm = TRUE)

# EXO 2

nrow(penguins[is.na(penguins$sex), ])

sum(is.na(penguins$sex))

penguins_from_adelie_dream <- penguins[penguins$island == "Dream" | penguins$island == "Adelie", ]

max(penguins_from_adelie_dream$body_mass_g, na.rm = TRUE)

is_penguin_overweight <- penguins$body_mass_g > 4000

mean(is_penguin_overweight == TRUE, na.rm = TRUE)

# EXO 3
data(sleep)
head(sleep)

g1 <- sleep[sleep$group == 1, ]$extra

g2 <- sleep[sleep$group == 2, ]$extra

annotated_histogram <- function(data, name) {
  hist(data, probability = TRUE, main = name)
  abline(v = mean(data), col = "red")
  abline(v = median(data), col = "orange") 
  abline(v = quantile(data, 0.25), col = "blue")
  abline(v = quantile(data, 0.75), col = "blue")
  legend("topright",
         legend = c("moyenne", "médiane", "quartiles"),
         col = c("red", "orange", "blue"),
         lty = 1)
}

annotated_histogram(g1, "Histogram of G1")
annotated_histogram(g2, "Histogram of G2")

plot(density(g1), main = "Densité de G1", col = "red", xlim = c(-4, 9))
lines(density(g2), col = "blue")


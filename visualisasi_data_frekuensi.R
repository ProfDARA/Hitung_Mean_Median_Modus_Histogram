#044112738 Danang Agung RA UPBJJ Yogyakarta

# Data diisikan disini
Kategori <- c("18-22", "23-27", "28-32", "33-37", "38-42", "43-47", "48-52")
Frekuensi <- c(265 88, 108, 19, 39, 44, 5)

# kalkulasi mean, median, and modus
# membuat vektor per kategori
kategori_rep <- numeric(sum(Frekuensi))
index <- 1
for (i in 1:length(Kategori)) {
  for (j in 1:Frekuensi[i]) {
    kategori_rep[index] <- mean(as.numeric(strsplit(Kategori[i], "-")[[1]]))
    index <- index + 1
  }
}

# kalkulasi median
median_data <- median(kategori_rep)

# kalkulasi mean
mean_data <- sum(as.numeric(unlist(strsplit(Kategori, "-"))[1:length(Kategori)]) * Frekuensi) / sum(Frekuensi)

# Fungsi kalkulasi modus
modus <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
modus_data <- modus(kategori_rep)

# Print median, mean, and modus di terminal
cat("Median:", median_data, "\n")
cat("Mean:", mean_data, "\n")
cat("Modus:", modus_data, "\n")


# Original plot
par(mfrow = c(1, 3)) # pembagian halaman menjadi 3 bagian

# Plot 1: Statistik
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
text(0.5, 0.9, paste("Mean: ", round(mean_data, 2)), cex = 2.8)
text(0.5, 0.7, paste("Median: ", median_data), cex = 2.8)
text(0.5, 0.5, paste("Modus: ", modus_data), cex = 2.8)
text("044112738  Danang Agung RA", cex = 1.5, x = 0.5, y = 0.1, col = "black")
title(main = "Statistik")

# Plot 2: Polygon
plot(1:length(Kategori), Frekuensi, type = "n", xaxt = "n", xlab = "Kategori", ylab = "Frekuensi", ylim = c(0, max(Frekuensi)*1.2))
axis(1, at = 1:length(Kategori), labels = Kategori, las = 2)
polygon(c(1:length(Kategori), rev(1:length(Kategori))), c(Frekuensi, rep(0, length(Frekuensi))), col = "skyblue", border = NA)
title(main = "Polygon Plot")

# Plot 3: Histogram
# pembuatan histogram break, karena tak ada data antara 22 dan 23
breaks <- seq(0.5, length(Kategori)+0.5, by = 1)
# Plot histogram
barplot(Frekuensi, main = "Histogram", xlab = "Kategori", ylab = "Frekuensi", names.arg = Kategori, col = "skyblue", border = "black", space = 0, ylim = c(0, max(Frekuensi)*1.2))
title(main = "Histogram")



# Reset layout
par(mfrow = c(1, 1))
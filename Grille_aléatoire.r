# Define the number of rows, columns, and mines
nrows <- 10
ncols <- 10
nmines <- 10

# Generate a matrix of zeros
matrix <- matrix(0, nrow = nrows, ncol = ncols)

# Generate random mine locations
mine_locs <- sample(nrows * ncols, nmines)

# Assign mines to the matrix
matrix[mine_locs] <- -1

# Generate a function to count mines in neighboring cells
count_mines <- function(matrix, row, col) {
  if (matrix[row, col] == -1) {
    return(-1)
  } else {
    rows <- c((row-1):(row+1))
    cols <- c((col-1):(col+1))
    rows <- rows[rows >= 1 & rows <= nrows]
    cols <- cols[cols >= 1 & cols <= ncols]
    return(sum(matrix[rows, cols] == -1))
  }
}

# Generate a function to generate the final grid with mine counts
generate_grid <- function(matrix) {
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      if (matrix[i, j] != -1) {
        matrix[i, j] <- count_mines(matrix, i, j)
      }
    }
  }
  return(matrix)
}

# Generate the final grid with mine counts
final_grid <- generate_grid(matrix)

# Affichage de contrôle de la grille
print(final_grid)

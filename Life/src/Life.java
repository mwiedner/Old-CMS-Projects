/**
 * A class implementing the rules of Life
 * 
 * Complete the class according to the instructions in the project write-up.
 *
 */

public class Life {

	private boolean[][] grid;

	
	// Incomplete constructor
	public Life(int nRows, int nCols) {
		this.grid = new boolean[40][40];
	}
	
	
	// Clear stuff
	public void clear() {
		for (int row = 0; row < grid.length; row++) {
			for (int col = 0; col < grid.length; col++) {
				this.grid[row][col] = false;
			}
		}
	}
	
	// Set a cell true
	public void set(int r, int c) {
		this.grid[r][c] = true;
	}
	
	
	// Test the state of a cell. Return false if it does not exist.
	public boolean isAlive(int r, int c) {
		if (r < 0 || c < 0 || r > grid.length - 1 || c > grid.length - 1) {
			return false;
		}
		else if (this.grid[r][c] == true) {
			return true;
		}
		else {
			return false;
		}
	}
	
	
	// Return number of rows
	public int numRows() {
		return grid.length;
	}
	
	// Return number of columns
	public int numCols() {
		return grid.length;
	}
	
	
	// Loop through every cell and count the number of neighbors. Treat cell accordingly based on the number of neighbors
	public void update() {
		boolean[][] next = new boolean[grid.length][grid.length];  // Temporary grid in order to allow cells to change simultaneously
		for (int row = 0; row < grid.length; row++) {
			for (int col = 0; col < grid.length; col++) {
				for (int n = 0; n <= 8; n++) {
					int neighbors = 0;  // Number of neighbors
					
					// If statements for each neighbor to see if it is alive or not
					if (isAlive(row - 1, col - 1) && this.grid[row - 1][col - 1]) {
						neighbors++;
					}
					if (isAlive(row - 1, col) && this.grid[row - 1][col]) {
						neighbors++;
					}
					if (isAlive(row - 1, col + 1) && this.grid[row - 1][col + 1]) {
						neighbors++;
					}
					if (isAlive(row, col - 1) && this.grid[row][col - 1]) {
						neighbors++;
					}
					if (isAlive(row, col + 1) && this.grid[row][col + 1]) {
						neighbors++;
					}
					if (isAlive(row + 1, col - 1) && this.grid[row + 1][col - 1]) {
						neighbors++;
					}
					if (isAlive(row + 1, col) && this.grid[row + 1][col]) {
						neighbors++;
					}
					if (isAlive(row + 1, col + 1) && this.grid[row + 1][col + 1]) {
						neighbors++;
					}
					
					// if statements for implementing survival conditions
					if (neighbors < 2 && this.grid[row][col]) {
						next[row][col] = false;
					}
					else if ((neighbors == 2 || neighbors == 3) && this.grid[row][col]) {
						next[row][col] = true;
					}
					else if (neighbors > 3 && this.grid[row][col]) {
						next[row][col] = false;
					}
					else if (neighbors == 3 && this.grid[row][col] == false) {
						next[row][col] = true;
					}
				}
			}
		}
		this.grid = next;  // Turn the temporary grid into the main one
	}
}

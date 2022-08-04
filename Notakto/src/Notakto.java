/**
 * Play Notakto
 * 
 * Notakto is like tic-tac-toe but only uses X's.
 * 
 * The first player who completes a line of X's is the LOSER.
 * 
 * @author CMS 170, Spring 2020
 *
 */


import java.util.Scanner;

public class Notakto {
	
	private boolean[][] board;  // true implies X at the location and false implies none
	private Scanner scan;  // Use the scanner for input
	
	
	/**
	 * Constructor -- initializes board size and input Scanner
	 */
	public Notakto() {
		this.board = new boolean[3][3];
		this.scan = new Scanner(System.in);
	}
	
	
	/**
	 * Print the board
	 */
	public void print() {
		// Use an outer loop that goes over the rows
		for (int r = 0; r < 3; r++) {
			// inner loop that goes over columns
			System.out.println(" "); // Leading space to center the values in their squares
			for (int c = 0; c < 3; c++) {
				// If board[r][c] is true then an X has been marked in that spot
				if (c == 0) {
					System.out.print(" ");
				}
				if (board[r][c]) {
					System.out.print("X");
				}
				// Other case, we want to print the number corresponding to the location on the board
				else {
					System.out.print(3 * r + c + 1);
				}
				if (c < 2) {
				System.out.print(" | ");
				}
				if (r == 2 && c == 2) {
					System.out.println();
				}
			}
			if (r < 2) {
			System.out.print("\n-----------"); // Move to the next row
			}
		}
	}
	
	
	/**
	 * Read a move
	 * Tests if the move is already taken
	 * Adjust boolean values of the array, thus replacing the number with an X in the print() method
	 * Also, returns true if the move that was read is a valid move
	 * If invalid, returns false
	 */
	public boolean readMove(int n) {
	 if (n == 1 && board[0][0] == false) {
		 board[0][0] = true;
		 return true;
	 }
	 if (n == 2 && board[0][1] == false) {
		 board[0][1] = true;
		 return true;
	 }
	 if (n == 3 && board[0][2] == false) {
		 board[0][2] = true;
		 return true;
	 }
	 if (n == 4 && board[1][0] == false) {
		 board[1][0] = true;
		 return true;
	 }
	 if (n == 5 && board[1][1] == false) {
		 board[1][1] = true;
		 return true;
	 }
	 if (n == 6 && board[1][2] == false) {
		 board[1][2] = true;
		 return true;
	 }
	 if (n == 7 && board[2][0] == false) {
		 board[2][0] = true;
		 return true;
	 }
	 if (n == 8 && board[2][1] == false) {
		 board[2][1] = true;
		 return true;
	 }
	 if (n == 9 && board[2][2] == false) {
		 board[2][2] = true;
		 return true;
	 }
	 else {
		 return false;
	 }
	}
	
	/**
	 * Check to see if there is a line of X's
	 * Returns true if a line is made, false if not
	 */
	public boolean lineCheck() {
		if (board[0][0] && board[0][1] && board[0][2]) {
			return true;
		}
		else if (board[1][0] && board[1][1] && board[1][2]) {
			return true;
		}
		else if (board[2][0] && board[2][1] && board[2][2]) {
			return true;
		}
		else if (board[0][0] && board[1][0] && board[2][0]) {
			return true;
		}
		else if (board[0][1] && board[1][1] && board[2][1]) {
			return true;
		}
		else if (board[0][2] && board[1][2] && board[2][2]) {
			return true;
		}
		else if (board[0][0] && board[1][1] && board[2][2]) {
			return true;
		}
		else if (board[2][0] && board[1][1] && board[0][2]) {
			return true;
		}
		else {
			return false;
		}
	}
	
	
	/**
	 * Play the game -- will contain the main game loop and logic
	 */
	public void play() {
		
		boolean playing = true;
		int player = 1; // Current player - used to declare winner and whose turn it is
		System.out.println("Welcome to Notakto! It is Player 1's turn. Here is the board: ");
		
		while (playing) {
			System.out.println("\n"); // Prints blank space to decrease clutter in the console
			this.print(); // Print the board
			
			// Prompt the user to select a move
			boolean gettingMove = true;
			while (gettingMove) {
				// Print a message asking the user to pick an open space
				System.out.println("\nPlayer " + player + ", pick an open space:");
				
				// Read the response using this.scan
				int response = this.scan.nextInt();
			
				if (readMove(response)) { // If the response is valid, then readMove(response) will return true and the gettingMove loop will end and switch player turns
					gettingMove = false;
				}
				else { // If the above if statement failed, then an invalid move was made
					System.out.println("That is an invalid move. Try again."); // Print error message
					this.print(); // Reprint the board
				}
			}
			
			// Switch players
			if (player == 1) {
				player = 2;
			}
			else {
				player = 1;
			}
			
			// Check if a line has been made
			if (lineCheck()) {
				if (player == 1) {
					System.out.println("Player 2 made a line!\nPlayer 1 has won! Good game!"); // Winning message for Player 1 win
				}
				else {
					System.out.println("Player 1 made a line!\nPlayer 2 has won! Good game!"); // Winning message for Player 2 win
				}
				playing = false; // End the game
				}
			}
		}
		
		
	
	

	/**
	 * Main -- create a new Notakto object and start the game
	 *  Don't add any code to main. All game logic will go in the play method
	 * @param args
	 */
	public static void main(String[] args) {
		Notakto notakto = new Notakto();
		notakto.play();
	}

}

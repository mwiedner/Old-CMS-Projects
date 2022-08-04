// SNAKE
// CMS 167, Spring 2019

public class SnakeGame {

    public static void main(String[] args) {
        Snake snake = new Snake();
        Square pellet = new Square(.90, .90);
        
        boolean eaten = false;
           
        boolean playing = true;
        while (playing) {
             
            // Move the snake
            // Add a new segment if the snake eats the pellet
            snake.update(pellet);
             
            // Did the snake hit itself?
            if (snake.collidesWithSelf()) {
                playing = false;    
            }
            
            // Draw
            StdDraw.clear();
            
            snake.draw();
            pellet.draw();
             
            StdDraw.show(100);
        }
    }
}
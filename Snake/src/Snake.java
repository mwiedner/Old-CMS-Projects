import java.util.ArrayList;
import java.awt.event.KeyEvent;

public class Snake
{
    private ArrayList<Square> body;
    private String direction;
    
    //*** Construct a new Snake ***//
    //
    // Default Snake has two segments and is moving right
    public Snake() {
        this.body = new ArrayList<Square>();
        this.body.add(new Square(.5, .5));
        this.body.add(new Square(.45, .50));
        this.direction = "right";
    }
    
    //*** Move the Snake ***//
    //
    // Increase the length if the Snake collides with the pellet
    public void update(Square pellet) {

    	// Controls
    	if (StdDraw.isKeyPressed(KeyEvent.VK_RIGHT)) {
            this.direction = "right";
        }
    	if (StdDraw.isKeyPressed(KeyEvent.VK_LEFT)) {
            this.direction = "left";
        }
    	if (StdDraw.isKeyPressed(KeyEvent.VK_UP)) {
            this.direction = "up";
        }
    	if (StdDraw.isKeyPressed(KeyEvent.VK_DOWN)) {
            this.direction = "down";
        }
    	
        // Get the current head
        Square head = this.body.get(0);
        
        double newHeadX = 0.0;
        double newHeadY = 0.0;

        // Move right
        if (this.direction.equals("right")) {
            newHeadX = head.getX() + head.RADIUS * 2;
            newHeadY = head.getY();
        }
        
        // Move left
        if (this.direction.equals("left")) {
        	newHeadX = head.getX() - head.RADIUS * 2;
        	newHeadY = head.getY();
        }
        
        // Move up
        if (this.direction.equals("up")) {
        	newHeadX = head.getX();
        	newHeadY = head.getY() + head.RADIUS * 2;
        }
        
        // Move down
        if (this.direction.equals("down")) {
        	newHeadX = head.getX();
        	newHeadY = head.getY() - head.RADIUS * 2;
        }
        
        // Add the new head segment
        // Recall: position 0 is the first position
        // This add method puts a new element at position 0
        this.body.add(0, new Square(newHeadX, newHeadY));
        
        // Remove the old tail segment
        // Recall: this.body.size() - 1 must be the last element
        if (!head.collidesWith(pellet)) {
            this.body.remove(this.body.size() - 1);
        } else {
        	pellet.newLocation();
        }
        
    }
    
    //*** Return true if this Snake's head hits one of its segments ***//
    public boolean collidesWithSelf() {
        Square head = this.body.get(0);
            
        // Loop over other body segments
        // Loop starts at index 1 to avoid comparing head to itself
        for (int i = 1; i < this.body.size(); i++) {
            if (head.collidesWith(this.body.get(i))){
                return true;   
            }
        }
        
        // If no segments collided, return false
        return false;
    }
    
    public void draw() {
        for (int i = 0; i < this.body.size(); i++) {
            this.body.get(i).draw();
        }
    }
}
public class Square {
    
    private double x;
    private double y;
    final double RADIUS = .025;
    
    public Square(double newX, double newY) {
        this.x = newX;
        this.y = newY;
    }
   
    public void draw() {
        StdDraw.setPenColor(0, 0, 0);
        StdDraw.filledSquare(this.x, this.y, this.RADIUS);
    }
    
    //*** Get the center and edge positions ***//
    //
    // The edge positions are inset just a little from their true locations
    // This prevents accidental overlaps for contiguous squares
    
    public double getX() {
        return this.x;   
    }
    
    public double getY() {
        return this.y;
    }
    
    public double right() {
        return this.x + this.RADIUS - .001;
    }
    
    public double left() {
        return this.x - this.RADIUS + .001;
    }
    
    public double top() {
        return this.y + this.RADIUS - .001;
    }
    
    public double bottom() {
        return this.y - this.RADIUS + .001;
    }
    
    //*** Check for collisions with another square ***//
    public boolean collidesWith(Square other) {
    
        if (this.right() <= other.left()) {
            return false;
        }
        
        if (this.left() >= other.right()) {
            return false;   
        }
        
        if (this.top() <= other.bottom()) {
            return false;   
        }
        
        if (this.bottom() >= other.top()) {
            return false;
        }
        
        return true;
    }
    
    //*** Reposition this square at a new random spot ***//
    public void newLocation() {
        this.x = Math.random();
        this.y = Math.random();
    }
    
}
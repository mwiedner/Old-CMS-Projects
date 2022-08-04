public class TrafficLight implements ColorChanging {

    // Data members
    private String road1;
    private String road2;
    private String color;
    private String cardinalDirection;

    // Constructor
    public TrafficLight(String r1, String r2, String c, String d) {
        road1 = r1;
        road2 = r2;
        color = c;
        cardinalDirection = d;
    }

    // Getters and setters
    public String getRoad1() {
        return road1;
    }
    public void setRoad1(String road1) {
        this.road1 = road1;
    }

    public String getRoad2() {
        return road2;
    }
    public void setRoad2(String road2) {
        this.road2 = road2;
    }

    public String getColor() {
        return color;
    }
    public void setColor(String color) {
        this.color = color;
    }

    public String getCardinalDirection() {
        return cardinalDirection;
    }
    public void setCardinalDirection(String cardinalDirection) {
        this.cardinalDirection = cardinalDirection;
    }

    // Methods
    public void change() { // Method that overrides the change() method implemented from the ColorChanging interface.
        if (getColor().equals("red")) { // If the color is red, then the color changes to green
            setColor("green");
        }
        else if (getColor().equals("green")) { // If the color is green, then the color changes to yellow
            setColor("yellow");
        }
        else if (getColor().equals("yellow")) { // If the color is yellow, then the color changes to red
            setColor("red");
        }
        System.out.println("Flash!"); // Print statement to indicate a color change
    }

    public void info() { // Large print statement to state the information of the traffic light
        System.out.println("The traffic light on the " + getCardinalDirection() + " side of " + getRoad1() + " and " + getRoad2() + " is currently " + getColor() + ".");
    }
}
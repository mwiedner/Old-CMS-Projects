import java.util.Random; // Needed to randomize the salamander's color

public class Salamander implements ColorChanging {
    Random rng = new Random();

    // Data Members
    private String name;
    private int age;
    private String habitat;
    private String color;

    // Constructor
    public Salamander(String s, int a, String h, String c) {
        name = s;
        age = a;
        habitat = h;
        color = c;
    }

    // Getters and setters
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }
    public void setAge(int age) {
        this.age = age;
    }

    public String getHabitat() {
        return habitat;
    }
    public void setHabitat(String habitat) {
        this.habitat = habitat;
    }

    public String getColor() {
        return color;
    }
    public void setColor(String color) {
        this.color = color;
    }

    // Methods
    public void change() { // When given no parameters, the salamander randomly changes to one of 9 colors.
        int randomize = rng.nextInt(9);
        if (randomize == 0) {
            setColor("red");
        }
        else if (randomize == 1) {
            setColor("orange");
        }
        else if (randomize == 2) {
            setColor("yellow");
        }
        else if (randomize == 3) {
            setColor("green");
        }
        else if (randomize == 4) {
            setColor("blue");
        }
        else if (randomize == 5) {
            setColor("purple");
        }
        else if (randomize == 6) {
            setColor("black");
        }
        else if (randomize == 7) {
            setColor("white");
        }
        else if (randomize == 8) {
            setColor("brown");
        }
        System.out.println("Morph!"); // Print statement to indicate a color change
    }

    public void change(String color) { // Overload the change() method so that it can take a string as a parameter to manually set the color
        setColor(color);
        System.out.println("Morph!"); // Print statement to indicate a color change
    }

    public void info() { // Large print statement that states all of the information of the salamander
        System.out.println("This salamander's name is " + getName() + ". They are " + getAge() + " years old and live in " + getHabitat() + ". Currently, they are " + getColor() + ".");
    }

    public void color() { // Smaller print statement that only states the color of the salamander
        System.out.println(getName() + " is currently " + getColor() + ".");
    }
}
public class Driver {
    public static void main(String[] args) {
        Salamander Alan = new Salamander("Alan", 2, "Miami Zoo", "blue");
        TrafficLight NorthSideFairbanksPark = new TrafficLight("Fairbanks Avenue", "Park Avenue", "red", "North");
        
        Alan.info();
        Alan.change("red");
        Alan.color();
        Alan.change();
        Alan.color();

        NorthSideFairbanksPark.info();
        NorthSideFairbanksPark.change();
        NorthSideFairbanksPark.info();
    }
}
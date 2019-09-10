public class Inventory {
    int smallShirt = 0;
    int mediumShirt = 0;
    int largeShirt = 0;
    int cap = 0;

    public Inventory(int s, int m, int l, int c) {
        smallShirt = s;
        mediumShirt = m;
        largeShirt = l;
        cap = c;
    }

    synchronized void processSmallShirt(Order newOrder) {
        // this.displayInventory();
        if (this.smallShirt >= newOrder.quantity) {
            this.smallShirt -= newOrder.quantity;
            newOrder.isSuccessful = true;
            System.out.println("Order " +  newOrder.orderID + " is successful");
        }
        else {
            System.out.println("Order " + newOrder.orderID + " is failed");
        }
        this.displayInventory();
    }

    synchronized void processMediumShirt(Order newOrder) {
        // this.displayInventory();
        if (this.mediumShirt >= newOrder.quantity) {
            this.mediumShirt -= newOrder.quantity;
            newOrder.isSuccessful = true;
            System.out.println("Order " +  newOrder.orderID + " is successful");
        }
        else {
            System.out.println("Order " + newOrder.orderID + " is failed");
        }
        this.displayInventory();
    }

    synchronized void processLargeShirt(Order newOrder) {
        // this.displayInventory();
        if (this.largeShirt >= newOrder.quantity) {
            this.largeShirt -= newOrder.quantity;
            newOrder.isSuccessful = true;
            System.out.println("Order " +  newOrder.orderID + " is successful");
        }
        else {
            System.out.println("Order " + newOrder.orderID + " is failed");
        }
        this.displayInventory();
    }

    synchronized void processCap(Order newOrder) {
        // this.displayInventory();
        if (this.cap >= newOrder.quantity) {
            this.cap -= newOrder.quantity;
            newOrder.isSuccessful = true;
            System.out.println("Order " +  newOrder.orderID + " is successful");
        }
        else {
            System.out.println("Order " + newOrder.orderID + " is failed");
        }
        this.displayInventory();
    }

    public void processOrder(Order newOrder) {
        if (newOrder.type == Order.Type.SMALLSHIRT) {
            this.processSmallShirt(newOrder);
        }
        else if (newOrder.type == Order.Type.MEDIUMSHIRT) {
            this.processMediumShirt(newOrder);
        }
        else if (newOrder.type == Order.Type.LARGESHIRT) {
            this.processLargeShirt(newOrder);
        }
        else if (newOrder.type == Order.Type.CAP) {
            this.processCap(newOrder);
        }
    }

    public void displayInventory() {
        System.out.println("Inventory:  ");
        System.out.println("S: " + smallShirt);
        System.out.println("M: " + mediumShirt);
        System.out.println("L: " + largeShirt);
        System.out.println("C: " + cap);
    }
}
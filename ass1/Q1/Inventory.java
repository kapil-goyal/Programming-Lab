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

    synchronized void printOrderStatus(boolean status, int orderID) {
        this.displayInventory();
        if (status) {
            System.out.println("Order " +  orderID + " is successful");
            System.out.println();
        }
        else {
            System.out.println("Order " + orderID + " is failed");
            System.out.println();
        }
    }

    synchronized void processSmallShirt(Order newOrder) {
        if (this.smallShirt >= newOrder.quantity) {
            this.printOrderStatus(true, newOrder.orderID);
            this.smallShirt -= newOrder.quantity;
        }
        else {
            this.printOrderStatus(false, newOrder.orderID);
        }
    }

    synchronized void processMediumShirt(Order newOrder) {
        if (this.mediumShirt >= newOrder.quantity) {
            this.printOrderStatus(true, newOrder.orderID);
            this.mediumShirt -= newOrder.quantity;
        }
        else {
            this.printOrderStatus(false, newOrder.orderID);
        }
    }

    synchronized void processLargeShirt(Order newOrder) {
        if (this.largeShirt >= newOrder.quantity) {
            this.printOrderStatus(true, newOrder.orderID);
            this.largeShirt -= newOrder.quantity;
        }
        else {
            this.printOrderStatus(false, newOrder.orderID);
        }
    }

    synchronized void processCap(Order newOrder) {
        if (this.cap >= newOrder.quantity) {
            this.printOrderStatus(true, newOrder.orderID);
            this.cap -= newOrder.quantity;
        }
        else {
            this.printOrderStatus(false, newOrder.orderID);
        }
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
        System.out.println("|\tS\t|\tM\t|\tL\t|\tC\t|");
        System.out.println("|\t" + smallShirt + "\t|\t" + mediumShirt + "\t|\t" + largeShirt + "\t|\t" + cap + "\t|");
        System.out.println();
    }
}
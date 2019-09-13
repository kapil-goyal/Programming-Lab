// stores and updates quantity of products in the inventory
public class Inventory {
    int smallShirt = 0;
    int mediumShirt = 0;
    int largeShirt = 0;
    int cap = 0;

    // contructor for Inventory
    public Inventory(int s, int m, int l, int c) {
        smallShirt = s;
        mediumShirt = m;
        largeShirt = l;
        cap = c;
    }

    // print status of order i.e. successful or failed
    synchronized void printOrderStatus(boolean status, int orderID) {
        //print current contents of inventory
        this.displayInventory();
        // check order status
        if (status) {
            //print order successful
            System.out.println("Order " + orderID + " is successful");
            System.out.println();
        } else {
            //print order failed
            System.out.println("Order " + orderID + " is failed");
            System.out.println();
        }
    }

    // handle order of small size shirt
    synchronized void processSmallShirt(Order newOrder) {
        // checks if order can be completed
        if (this.smallShirt >= newOrder.quantity) {
            // call method to print successful order
            this.printOrderStatus(true, newOrder.orderID);
            //update inventory
            this.smallShirt -= newOrder.quantity;
        } else {
            // call method to print failed order
            this.printOrderStatus(false, newOrder.orderID);
        }
    }

    // handle order of medium size shirt
    synchronized void processMediumShirt(Order newOrder) {
        // checks if order can be completed
        if (this.mediumShirt >= newOrder.quantity) {
            // call method to print successful order
            this.printOrderStatus(true, newOrder.orderID);
            //update inventory
            this.mediumShirt -= newOrder.quantity;
        } else {
            // call method to print failed order
            this.printOrderStatus(false, newOrder.orderID);
        }
    }

    // handle order of large size shirt
    synchronized void processLargeShirt(Order newOrder) {
        // checks if order can be completed
        if (this.largeShirt >= newOrder.quantity) {
            // call method to print successful order
            this.printOrderStatus(true, newOrder.orderID);
            //update inventory
            this.largeShirt -= newOrder.quantity;
        } else {
            // call method to print failed order
            this.printOrderStatus(false, newOrder.orderID);
        }
    }

    // handle order of cap
    synchronized void processCap(Order newOrder) {
        // checks if order can be completed
        if (this.cap >= newOrder.quantity) {
            // call method to print successful order
            this.printOrderStatus(true, newOrder.orderID);
            //update inventory
            this.cap -= newOrder.quantity;
        } else {
            // call method to print failed order
            this.printOrderStatus(false, newOrder.orderID);
        }
    }

    // processes all orders
    public void processOrder(Order newOrder) {
        // checks product type and calls corresponding methods to process order
        if (newOrder.type == Order.Type.SMALLSHIRT) {
            this.processSmallShirt(newOrder);
        } else if (newOrder.type == Order.Type.MEDIUMSHIRT) {
            this.processMediumShirt(newOrder);
        } else if (newOrder.type == Order.Type.LARGESHIRT) {
            this.processLargeShirt(newOrder);
        } else if (newOrder.type == Order.Type.CAP) {
            this.processCap(newOrder);
        }
    }

    // display the contents of the inventory
    public void displayInventory() {
        System.out.println("Inventory:  ");
        System.out.println("|\tS\t|\tM\t|\tL\t|\tC\t|");
        System.out.println("|\t" + smallShirt + "\t|\t" + mediumShirt + "\t|\t" + largeShirt + "\t|\t" + cap + "\t|");
        System.out.println();
    }
}
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


class WorkerThread implements Runnable {
    Inventory myInventory;
    Order myOrder;

    WorkerThread(Inventory myInventory, Order myOrder) {
        this.myInventory = myInventory;
        this.myOrder = myOrder;
    }

    public void run() {
        myInventory.processOrder(myOrder);
    }
}

class Main {
    public static void main(String[] args) {
        int smallShirt;
        int mediumShirt;
        int largeShirt;
        int cap;
        // takes and stores initial inventory status
        Scanner input = new Scanner(System.in);
        System.out.print("Enter small shirts in inventory: ");
        smallShirt = input.nextInt();
        System.out.print("Enter medium shirts in inventory: ");
        mediumShirt = input.nextInt();
        System.out.print("Enter large shirts in inventory: ");
        largeShirt = input.nextInt();
        System.out.print("Enter cap in inventory: ");
        cap = input.nextInt();

        //create new inventory object with given inputs
        Inventory myInventory = new Inventory(smallShirt, mediumShirt, largeShirt, cap);

        //takes input number of orders
        System.out.println("Enter Number of Students ordering");
        int orders = input.nextInt();
        //creates array of order object with input
        Order[] Orders = new Order[orders];

        System.out.println("Enter Orders");

        //takes orders input and stores them
        for (int i = 0; i < orders; i++) {
            System.out.print((i + 1) + " ");
            char type;
            int quantity;
            int orderid = i + 1;
            type = input.next().charAt(0);
            quantity = input.nextInt();

            Order newOrder = new Order(orderid, type, quantity);
            Orders[i] = newOrder;
        }

        // creates a thread pool of 10 threads
        ExecutorService executor = Executors.newFixedThreadPool(10);

        // for each order in orders list
        for (int i = 0; i < orders; i++) {

            // Make worker thread runnable for ith order
            Runnable worker = new WorkerThread(myInventory, Orders[i]);

            // assign this order to thread pool
            executor.execute(worker);
        }

        //completes allocated tasks and terminates the executor
        executor.shutdown();

        //waits for the executor to terminate
        while (!executor.isTerminated()) {
        }
    }

}
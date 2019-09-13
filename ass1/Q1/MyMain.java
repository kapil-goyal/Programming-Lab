import java.util.*;
import java.util.concurrent.ExecutorService;  
import java.util.concurrent.Executors; 

class WorkerThread implements Runnable {
    Inventory myInventory;
    Order myOrder;
    WorkerThread (Inventory myInventory, Order myOrder) {
        this.myInventory = myInventory;
        this.myOrder = myOrder;
    }

    public void run () {
        myInventory.processOrder(myOrder);
    }
}

class MyMain {
    public static void main(String[] args) {
        int smallShirt;
        int mediumShirt;
        int largeShirt;
        int cap;
        Scanner input = new Scanner(System.in);
        System.out.print("Enter small shirts in inventory: ");
        smallShirt = input.nextInt();
        System.out.print("Enter medium shirts in inventory: ");
        mediumShirt = input.nextInt();
        System.out.print("Enter large shirts in inventory: ");
        largeShirt = input.nextInt();
        System.out.print("Enter cap in inventory: ");
        cap = input.nextInt();

        Inventory myInventory = new Inventory(smallShirt, mediumShirt, largeShirt, cap);

        System.out.println("Enter Number of Students ordering");
        int orders = input.nextInt();
        Order[] Orders = new Order[orders];

        System.out.println("Enter Orders");

        for (int i = 0; i < orders; i++) {
            System.out.print((i+1) + " ");
            char type;
            int quantity;
            int orderid = i+1;
            type = input.next().charAt(0);
            quantity = input.nextInt();

            Order newOrder = new Order(orderid, type, quantity);
            Orders[i] = newOrder;
        }

        ExecutorService executor = Executors.newFixedThreadPool(10);
        for (int i = 0; i < orders; i++) {
            Runnable worker = new WorkerThread(myInventory, Orders[i]);
            executor.execute(worker);
        }
        executor.shutdown();  
        while (!executor.isTerminated()) {   } 
    }

}
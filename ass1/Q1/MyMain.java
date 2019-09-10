import java.util.Scanner;

class MyThread extends Thread {
    Inventory myInventory;
    Order myOrder;
    MyThread (Inventory myInventory, Order myOrder) {
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
        MyThread[] Orders = new MyThread[orders];

        System.out.println("Enter Orders");

        for (int i = 0; i < orders; i++) {
            int orderid;
            char type;
            int quantity;
            orderid = input.nextInt();
            type = input.next().charAt(0);
            quantity = input.nextInt();

            Order newOrder = new Order(orderid, type, quantity);
            Orders[i] = new MyThread(myInventory, newOrder);
        }

        myInventory.displayInventory();

        for (int i = 0; i < orders; i++) {
            Orders[i].start();
        }
    }

}
// class containing details about an order
public class Order {
    // unique id of each order
    int orderID;
    // quantity of commodities ordered
    int quantity;
    // order status
    boolean isSuccessful;

    // available type of products
    enum Type {
        SMALLSHIRT,
        MEDIUMSHIRT,
        LARGESHIRT,
        CAP
    }

    // type of product ordered
    Type type;

    // constructor for creating a new order
    public Order(int id, char c, int quant) {
        orderID = id;
        quantity = quant;
        isSuccessful = false;
        switch (c) {
            case 'C':
                type = Type.CAP;
                break;
            case 'S':
                type = Type.SMALLSHIRT;
                break;
            case 'M':
                type = Type.MEDIUMSHIRT;
                break;
            case 'L':
                type = Type.LARGESHIRT;
                break;
        }
    }

    public void printDetails() {
        System.out.println("OrderID: " + orderID);
        System.out.println("Status: " + isSuccessful);
        System.out.println("Type: " + type);
        System.out.println("Quantity: " + quantity);
    }
}
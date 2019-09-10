public class Order {
    int orderID;
    int quantity;
    boolean isSuccessful;

    enum Type {
        SMALLSHIRT,
        MEDIUMSHIRT,
        LARGESHIRT,
        CAP
    }

    Type type;

    public Order(int id, char c, int quant) {
        orderID = id;
        quantity = quant;
        isSuccessful = false;
        switch(c) {
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
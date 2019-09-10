import java.util.*;  

class UnfinishedTray {
    boolean b1Pack, b1Seal;
    int b1, b2;

    public UnfinishedTray (int b1, int b2) {
        this.b1 = b1;
        this.b2 = b2;
        this.b1Pack = true;
        this.b1Seal = false;
    }

    public Bottle getNewBottleToPack() {
        if (b1 == 0 && b2 == 0) {
            return null;
        }
        if (b2 == 0) {
            b1--;
            return new Bottle(true, true);
        }
        if (b1 == 0) {
            b2--;
            return new Bottle(false, true);
        }
        if (b1Pack) {
            b1--;
            b1Pack = !b1Pack;
            return new Bottle(true, true);
        }
        else {
            b2--;
            b1Pack = !b1Pack;
            return new Bottle(false, true);
        }
    }

    public Bottle getNewBottleToSeal() {
        if (b1 == 0 && b2 == 0) {
            return null;
        }
        if (b2 == 0) {
            b1--;
            return new Bottle(true, false);
        }
        if (b1 == 0) {
            b2--;
            return new Bottle(false, false);
        }
        if (b1Seal) {
            b1--;
            b1Seal = !b1Seal;
            return new Bottle(true, false);
        }
        else {
            b2--;
            b1Seal = !b1Seal;
            return new Bottle(false, false);
        }
    }

    public boolean isEmpty() {
        return (b1 == 0 && b2 == 0)
    }

}

class Buffer {
    List<Bottle> tray;

    public Buffer() {
        tray = new ArrayList<Bottle>();
    }
}

class Main {
    public static void main (String[] args) {
        Scanner input = new Scanner(System.in);
        int numberOfB1 = input.nextInt();
        int numberOfB2 = input.nextInt();
        int observationTime = input.nextInt();
    }
}
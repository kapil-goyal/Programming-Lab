import java.util.*;

class UnfinishedTray {
    boolean b1Pack, b1Seal;
    Buffer b1Tray, b2Tray;

    public UnfinishedTray (Buffer b1Tray, Buffer b2Tray) {
        this.b1Tray = b1Tray;
        this.b2Tray = b2Tray;
        this.b1Pack = true;
        this.b1Seal = false;
    }

    public Bottle getNewBottleToPack() {
        if (b1Pack) {
            Bottle newBottle = b1Tray.getNewBottle();
            if (newBottle != null)
            {   
                b1Pack = !b1Pack;
                return newBottle;
            }
            else {
                return b2Tray.getNewBottle();
            }
        }
        else {
            Bottle newBottle = b2Tray.getNewBottle();
            if (newBottle != null)
            {   
                b1Pack = !b1Pack;
                return newBottle;
            }
            else {
                return b1Tray.getNewBottle();
            }
        }
    }

    public Bottle getNewBottleToSeal() {
        if (b1Seal) {
            Bottle newBottle = b1Tray.getNewBottle();
            if (newBottle != null)
            {   
                b1Seal = !b1Seal;
                return newBottle;
            }
            else {
                return b2Tray.getNewBottle();
            }
        }
        else {
            Bottle newBottle = b2Tray.getNewBottle();
            if (newBottle != null)
            {   
                b1Seal = !b1Seal;
                return newBottle;
            }
            else {
                return b1Tray.getNewBottle();
            }
        }
    }
}

class Buffer {
    Queue<Bottle> tray;

    public Buffer() {
        tray = new LinkedList<Bottle>();
    }

    synchronized Bottle getNewBottle() {
        if (this.getSize() > 0) {
            Bottle newBottle = tray.remove();
            return newBottle;
        }
        else {
            return null;
        }   
    }

    synchronized boolean addNewBottle(Bottle newBottle, int maxSize) {
        if (this.getSize() < maxSize) {
            tray.add(newBottle);
            return true;
        }
        else {
            return false;
        } 
    }

    public boolean isEmpty() {
        return (tray.size() == 0);
    }

    public int getSize() {
        return tray.size();
    }

    public void printTray() {
        System.out.println(tray);
    }
}

class MyPackagingThread extends Thread {
    int nextWakeupTimePack;
    int nextWakeupTimeSeal;
    int currentTime;
    Packaging packagingUnit;

    public MyPackagingThread(int currentTime, int nextWakeupTimePack, int nextWakeupTimeSeal, Packaging packagingUnit) {
        this.currentTime = currentTime;
        this.nextWakeupTimePack = nextWakeupTimePack;
        this.nextWakeupTimeSeal = nextWakeupTimeSeal;
        this.packagingUnit = packagingUnit;
    }

    public void run() {
        if (this.currentTime == this.nextWakeupTimePack) {
            int newWakeupTime = this.packagingUnit.Process(this.nextWakeupTimePack, this.nextWakeupTimeSeal);
            this.nextWakeupTimePack = newWakeupTime;
        }
    }
}

class MySealingThread extends Thread {
    int nextWakeupTimePack;
    int nextWakeupTimeSeal;
    int currentTime;
    Sealing sealingUnit;

    public MySealingThread(int currentTime, int nextWakeupTimePack, int nextWakeupTimeSeal, Sealing sealingUnit) {
        this.currentTime = currentTime;
        this.nextWakeupTimePack = nextWakeupTimePack;
        this.nextWakeupTimeSeal = nextWakeupTimeSeal;
        this.sealingUnit = sealingUnit;
    }

    public void run() {
        if (this.currentTime == this.nextWakeupTimeSeal) {
            int newWakeupTime = this.sealingUnit.Process(this.nextWakeupTimeSeal, this.nextWakeupTimePack);
            this.nextWakeupTimeSeal = newWakeupTime;
        }
    }
}

class Main {
    
    


    static void printBottleStatus(Buffer unfinishedB1,
                                    Buffer unfinishedB2,
                                    Buffer packagedB1,
                                    Buffer packagedB2,
                                    Buffer sealed,
                                    Buffer godownB1,
                                    Buffer godownB2,
                                    Packaging packagingUnit,
                                    Sealing sealingUnit) 
    {
        int countUnfinishedB1 = unfinishedB1.getSize();
        int countUnfinishedB2 = unfinishedB2.getSize();
        int countSealedB1 = packagedB1.getSize();
        int countSealedB2 = packagedB2.getSize();
        int countGodownB1 = godownB1.getSize();
        int countGodownB2 = godownB2.getSize();
        int countPackagedB1 = 0;
        int countPackagedB2 = 0;

        for (Bottle eachBottle : sealed.tray) {
            if (eachBottle.type == Bottle.Type.B1) {
                countPackagedB1++;
            }
            else {
                countPackagedB2++;
            }
        }

        if (packagingUnit.currentBottle != null) {
            if (packagingUnit.currentBottle.type == Bottle.Type.B1) {
                if (packagingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB1++;
                }
                else if (packagingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB1++;
                }
                else if (packagingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB1++;
                }
            }
            else {
                if (packagingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB2++;
                }
                else if (packagingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB2++;
                }
                else if (packagingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB2++;
                }
            }   
        }

        if (sealingUnit.currentBottle != null) {
            if (sealingUnit.currentBottle.type == Bottle.Type.B1) {
                if (sealingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB1++;
                }
                else if (sealingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB1++;
                }
                else if (sealingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB1++;
                }
            }
            else {
                if (sealingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB2++;
                }
                else if (sealingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB2++;
                }
                else if (sealingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB2++;
                }
            }   
        }

        System.out.println("Bottle Type   Status\t\t\tCount");
        System.out.println("B1            In Godown\t\t\t" + countGodownB1);
        System.out.println("B1            Packaged\t\t\t" + (countPackagedB1 + countGodownB1));
        System.out.println("B1            Sealed\t\t\t" + (countSealedB1 + countGodownB1));
        System.out.println("B1            Unfinished\t\t" + countUnfinishedB1);
        System.out.println("B2            In Godown\t\t\t" + countGodownB2);
        System.out.println("B2            Packaged\t\t\t" + (countPackagedB2 + countGodownB2));
        System.out.println("B2            Sealed\t\t\t" + (countSealedB2 + countGodownB2));
        System.out.println("B2            Unfinished\t\t" + countUnfinishedB2);       
    }

    public static void main (String[] args) throws InterruptedException {
        Scanner input = new Scanner(System.in);
        int numberOfB1 = input.nextInt();
        int numberOfB2 = input.nextInt();
        int observationTime = input.nextInt();

        Buffer unfinishedB1 = new Buffer();
        Buffer unfinishedB2 = new Buffer();
        Buffer packagedB1 = new Buffer();
        Buffer packagedB2 = new Buffer();
        Buffer sealed = new Buffer();
        Buffer godownB1 = new Buffer();
        Buffer godownB2 = new Buffer();

        Bottle tempBottle;
        for (int i = 0; i < numberOfB1; i++) {
            tempBottle = new Bottle(true);
            unfinishedB1.addNewBottle(tempBottle, Integer.MAX_VALUE);
        }

        for (int i = 0; i < numberOfB2; i++) {
            tempBottle = new Bottle(false);
            unfinishedB2.addNewBottle(tempBottle, Integer.MAX_VALUE);
        }

        UnfinishedTray unfinishedTray = new UnfinishedTray(unfinishedB1, unfinishedB2);
        Packaging packagingUnit = new Packaging(unfinishedTray, packagedB1, packagedB2, sealed, godownB1, godownB2);
        Sealing sealingUnit = new Sealing(unfinishedTray, packagedB1, packagedB2, sealed, godownB1, godownB2);

        int currentTime = 0, nextWakeupTimePack = 0, nextWakeupTimeSeal = 0;
        
        MyPackagingThread packagingThread;
        MySealingThread sealingThread;

        while (currentTime <= observationTime) {
            packagingThread = new MyPackagingThread(currentTime, nextWakeupTimePack, nextWakeupTimeSeal, packagingUnit);
            sealingThread = new MySealingThread(currentTime, nextWakeupTimePack, nextWakeupTimeSeal, sealingUnit); 

            packagingThread.start();
            sealingThread.start();
            packagingThread.join();
            sealingThread.join();

            nextWakeupTimePack = packagingThread.nextWakeupTimePack;
            nextWakeupTimeSeal = sealingThread.nextWakeupTimeSeal;
            currentTime = nextWakeupTimePack < nextWakeupTimeSeal ? nextWakeupTimePack : nextWakeupTimeSeal;
        }

        printBottleStatus(unfinishedB1, unfinishedB2, packagedB1, packagedB2, sealed, godownB1, godownB2, packagingUnit, sealingUnit);

    }
}
// Import libraries

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

// This class simulates unfinished tray
class UnfinishedTray {

    // these two booleans define
    // which bottle to send to sealing or packing unit
    boolean b1Pack, b1Seal;

    // Buffer queues for B1 bottles and B2 bottles
    Buffer b1Tray, b2Tray;

    // Constructor for unfinished tray
    public UnfinishedTray(Buffer b1Tray, Buffer b2Tray) {
        this.b1Tray = b1Tray;
        this.b2Tray = b2Tray;
        this.b1Pack = true;
        this.b1Seal = false;
    }

    // get new bottle for packing unit
    public Bottle getNewBottleToPack() {
        // if priority if b1
        if (b1Pack) {
            Bottle newBottle = b1Tray.getNewBottle();
            // get new bottle from b1 tray

            // if new b1 bottle is not available
            if (newBottle != null) {
                b1Pack = !b1Pack;
                return newBottle;
            }

            // then get new b2 bottle
            else {
                return b2Tray.getNewBottle();
            }
        }
        // if priority is b2
        else {
            Bottle newBottle = b2Tray.getNewBottle();
            // get new bottle from b1 tray

            // if new b2 bottle is not available
            if (newBottle != null) {
                b1Pack = !b1Pack;
                return newBottle;
            }

            // then get new b1 bottle
            else {
                return b1Tray.getNewBottle();
            }
        }
    }

    // get new bottle for sealing unit
    public Bottle getNewBottleToSeal() {
        // if priority if b1
        if (b1Seal) {
            Bottle newBottle = b1Tray.getNewBottle();
            // get new bottle from b1 tray

            // if new b1 bottle is not available
            if (newBottle != null) {
                b1Seal = !b1Seal;
                return newBottle;
            } else {
                // then get new b2 bottles
                return b2Tray.getNewBottle();
            }
        } else {
            Bottle newBottle = b2Tray.getNewBottle();
            // get new bottle from b1 tray

            // if new b2 bottle is not available
            if (newBottle != null) {
                b1Seal = !b1Seal;
                return newBottle;
            } else {
                // then get new b1 bottle
                return b1Tray.getNewBottle();
            }
        }
    }
}

// class for different buffers
class Buffer {
    // queue of bottles in a buffer
    Queue<Bottle> tray;
    // lock for synchronisation
    private final ReentrantLock bottleLock;

    // constructor for class initialises lock and queue
    public Buffer() {
        tray = new LinkedList<Bottle>();
        bottleLock = new ReentrantLock();
    }

    // remove bottle from a buffer
    public Bottle getNewBottle() {
        // acquire lock so that multiple do not remove
        // at the same time
        bottleLock.lock();
        // check if buffer contains bottles
        if (this.getSize() > 0) {
            // remove bottle from buffer
            Bottle newBottle = tray.remove();
            // release lock
            bottleLock.unlock();
            // return bottle
            return newBottle;
        } else {
            // release lock
            bottleLock.unlock();
            // return null if buffer is empty
            return null;
        }
    }

    // method to add bottle to a buffer
    public boolean addNewBottle(Bottle newBottle, int maxSize) {
        // acquire lock so that multiple do not add at the same time
        bottleLock.lock();
        // check if buffer is full
        if (this.getSize() < maxSize) {
            // add new bottle to the buffer
            tray.add(newBottle);
            // release lock
            bottleLock.unlock();
            // return true if bottle is added
            return true;
        } else {
            // release lock
            bottleLock.unlock();
            // return false if buffer is full
            return false;
        }
    }

    // method to check if buffer is empty
    public boolean isEmpty() {
        return (tray.size() == 0);
    }

    // method to get size of buffer
    public int getSize() {
        return tray.size();
    }

    // method to print contents of buffer
    public void printTray() {
        System.out.println(tray);
    }
}

// class to implememt thread for packaging
class MyPackagingThread extends Thread {
    // time to restart packaging thread
    int nextWakeupTimePack;
    // time to restart sealing thread
    int nextWakeupTimeSeal;
    // stores the current time
    int currentTime;
    Packaging packagingUnit;

    // constructor for thread class
    public MyPackagingThread(int currentTime, 
                            int nextWakeupTimePack, 
                            int nextWakeupTimeSeal, 
                            Packaging packagingUnit) {
        this.currentTime = currentTime;
        this.nextWakeupTimePack = nextWakeupTimePack;
        this.nextWakeupTimeSeal = nextWakeupTimeSeal;
        this.packagingUnit = packagingUnit;
    }

    public void run() {
        // check if it is time for thread to run
        if (this.currentTime == this.nextWakeupTimePack) {
            // run the packaging unit process
            int newWakeupTime = this.packagingUnit.Process(this.nextWakeupTimePack, 
                                                            this.nextWakeupTimeSeal);
            this.nextWakeupTimePack = newWakeupTime;
        }
    }
}

// class to implement thread for sealing
class MySealingThread extends Thread {
    // time to restart packaging thread
    int nextWakeupTimePack;
    // time to restart sealing thread
    int nextWakeupTimeSeal;
    // stores the current time
    int currentTime;
    Sealing sealingUnit;

    // constructor for thread class
    public MySealingThread(int currentTime, 
                            int nextWakeupTimePack, 
                            int nextWakeupTimeSeal, 
                            Sealing sealingUnit) {
        this.currentTime = currentTime;
        this.nextWakeupTimePack = nextWakeupTimePack;
        this.nextWakeupTimeSeal = nextWakeupTimeSeal;
        this.sealingUnit = sealingUnit;
    }

    public void run() {
        // check if it is time for thread to run
        if (this.currentTime == this.nextWakeupTimeSeal) {
            // run the sealing unit process
            int newWakeupTime = this.sealingUnit.Process(this.nextWakeupTimeSeal, 
                                                        this.nextWakeupTimePack);
            this.nextWakeupTimeSeal = newWakeupTime;
        }
    }
}

class Main {

    // method to print the current status of bottles
    static void printBottleStatus(Buffer unfinishedB1, Buffer unfinishedB2, 
                                    Buffer packagedB1, Buffer packagedB2,
                                    Buffer sealed, Buffer godownB1, Buffer godownB2, 
                                    Packaging packagingUnit, Sealing sealingUnit) {
        // store count of bottles in different buffers
        int countUnfinishedB1 = unfinishedB1.getSize();
        int countUnfinishedB2 = unfinishedB2.getSize();
        int countSealedB1 = packagedB1.getSize();
        int countSealedB2 = packagedB2.getSize();
        int countGodownB1 = godownB1.getSize();
        int countGodownB2 = godownB2.getSize();
        int countPackagedB1 = 0;
        int countPackagedB2 = 0;

        // store count of bottles in sealing buffer
        for (Bottle eachBottle : sealed.tray) {
            if (eachBottle.type == Bottle.Type.B1) {
                countPackagedB1++;
            } else {
                countPackagedB2++;
            }
        }

        // check if packaging unit is processsing a bottle
        if (packagingUnit.currentBottle != null) {
            // check type and status of bottle currently undergoing packaging and update
            // corrsponding counts
            if (packagingUnit.currentBottle.type == Bottle.Type.B1) {
                if (packagingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB1++;
                } else if (packagingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB1++;
                } else if (packagingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB1++;
                }
            } else {
                if (packagingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB2++;
                } else if (packagingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB2++;
                } else if (packagingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB2++;
                }
            }
        }

        // check if sealing unit is processing a bottle
        if (sealingUnit.currentBottle != null) {
            // check bottle type and state of the bottle currently in sealing unit and
            // update corresponding counts
            if (sealingUnit.currentBottle.type == Bottle.Type.B1) {
                if (sealingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB1++;
                } else if (sealingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB1++;
                } else if (sealingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB1++;
                }
            } else {
                if (sealingUnit.currentBottle.state == Bottle.State.UNFINISHED) {
                    countUnfinishedB2++;
                } else if (sealingUnit.currentBottle.state == Bottle.State.SEALED) {
                    countSealedB2++;
                } else if (sealingUnit.currentBottle.state == Bottle.State.PACKAGED) {
                    countPackagedB2++;
                }
            }
        }

        // print the bottles type and state-wise
        System.out.println("Bottle Type   Status\t\t\tCount");
        System.out.println("B1            Packaged\t\t\t" + (countPackagedB1 + countGodownB1));
        System.out.println("B1            Sealed\t\t\t" + (countSealedB1 + countGodownB1));
        System.out.println("B1            In Godown\t\t\t" + countGodownB1);
        // System.out.println("B1 Unfinished\t\t" + countUnfinishedB1);
        System.out.println("B2            Packaged\t\t\t" + (countPackagedB2 + countGodownB2));
        System.out.println("B2            Sealed\t\t\t" + (countSealedB2 + countGodownB2));
        System.out.println("B2            In Godown\t\t\t" + countGodownB2);
        // System.out.println("B2 Unfinished\t\t" + countUnfinishedB2);
    }

    public static void main(String[] args) throws InterruptedException {
        // input and store quantities of bottles and time
        Scanner input = new Scanner(System.in);
        int numberOfB1 = input.nextInt();
        int numberOfB2 = input.nextInt();
        int observationTime = input.nextInt();

        // initialise the different buffers
        Buffer unfinishedB1 = new Buffer();
        Buffer unfinishedB2 = new Buffer();
        Buffer packagedB1 = new Buffer();
        Buffer packagedB2 = new Buffer();
        Buffer sealed = new Buffer();
        Buffer godownB1 = new Buffer();
        Buffer godownB2 = new Buffer();

        Bottle tempBottle;
        // add B1 bottles to unfinished buffer B1
        for (int i = 0; i < numberOfB1; i++) {
            tempBottle = new Bottle(true);
            unfinishedB1.addNewBottle(tempBottle, Integer.MAX_VALUE);
        }
        // add B2 bottles to unfinished buffer B2
        for (int i = 0; i < numberOfB2; i++) {
            tempBottle = new Bottle(false);
            unfinishedB2.addNewBottle(tempBottle, Integer.MAX_VALUE);
        }

        // initialise unfinished tray, packaging unit and sealing unit
        UnfinishedTray unfinishedTray = new UnfinishedTray(unfinishedB1, unfinishedB2);
        Packaging packagingUnit = new Packaging(unfinishedTray, packagedB1, 
                                                packagedB2, sealed, godownB1, godownB2);
        Sealing sealingUnit = new Sealing(unfinishedTray, packagedB1, 
                                        packagedB2, sealed, godownB1, godownB2);

        // initialise the variables storing current time and time to restart packaging
        // and sealing threads
        int currentTime = 0, nextWakeupTimePack = 0, nextWakeupTimeSeal = 0;
        // thread for running packaging unit
        MyPackagingThread packagingThread;
        // thread for running sealing unit
        MySealingThread sealingThread;

        // loop while either bottles are processing or time for observation is not
        // reached
        while (currentTime <= observationTime && 
                (godownB1.getSize() < numberOfB1 || godownB2.getSize() < numberOfB2)) {
            // initialise packaging and sealing threads
            packagingThread = new MyPackagingThread(currentTime, 
                                                    nextWakeupTimePack, 
                                                    nextWakeupTimeSeal, 
                                                    packagingUnit);
            sealingThread = new MySealingThread(currentTime, 
                                                nextWakeupTimePack, 
                                                nextWakeupTimeSeal, 
                                                sealingUnit);

            // start the threads for packaging and sealing units
            packagingThread.start();
            sealingThread.start();
            // wait for threads to finish allocated tasks
            packagingThread.join();
            sealingThread.join();

            // update times to restart packaging and sealing threads
            nextWakeupTimePack = packagingThread.nextWakeupTimePack;
            nextWakeupTimeSeal = sealingThread.nextWakeupTimeSeal;
            // update current time
            currentTime = nextWakeupTimePack < nextWakeupTimeSeal ? nextWakeupTimePack : nextWakeupTimeSeal;
        }

        // print the current status of bottles
        printBottleStatus(unfinishedB1, unfinishedB2, packagedB1, packagedB2, sealed, godownB1, godownB2, packagingUnit,
                sealingUnit);

    }
}
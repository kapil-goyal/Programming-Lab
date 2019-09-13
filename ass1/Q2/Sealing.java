import java.util.*;

//handles the sealing operation
class Sealing {

    enum State {
        EMPTY,
        SEALING,
        WAITING
    }

    //state of packaging unit
    State state;
    //bottle currently in packaging unit
    Bottle currentBottle;
    //buffer of unfinished tray
    UnfinishedTray unfinishedTray;
    // packaging buffer of B1 sealed bottles 
    Buffer packagedB1;
    // packaging buffer of B2 sealed bottles
    Buffer packagedB2;
    // sealing buffer of packaged bottles
    Buffer sealed;
    // godown buffer of completed B1 bottles
    Buffer godownB1;
    // godown buffer of completed B2 bottles
    Buffer godownB2;

    //constructor for sealing class
    public Sealing(UnfinishedTray unfinishedTray,
                   Buffer packagedB1,
                   Buffer packagedB2,
                   Buffer sealed,
                   Buffer godownB1,
                   Buffer godownB2) {
        this.state = State.EMPTY;
        this.currentBottle = null;
        this.unfinishedTray = unfinishedTray;
        this.packagedB1 = packagedB1;
        this.packagedB2 = packagedB2;
        this.sealed = sealed;
        this.godownB1 = godownB1;
        this.godownB2 = godownB2;
    }

    //method to obtain new bottle when sealing unit is empty
    public void getNewBottle() {
        //obtain bottle from sealing buffer
        this.currentBottle = sealed.getNewBottle();
        if (this.currentBottle == null) {
            //obtain bottle from unfinished tray
            currentBottle = unfinishedTray.getNewBottleToSeal();
        }
    }

    //handles the processing of the sealing unit
    public int Process(int currentTime, int packNextWakeupTime) {
        //check if sealing has started
        if (this.state == State.SEALING) {
            //change state to waiting and return time of sealing
            this.state = State.WAITING;
            return currentTime + 3;
        }
        //check if sealing has finished
        else if (this.state == State.WAITING) {
            //check if bottle was already packaged
            if (this.currentBottle.state == Bottle.State.PACKAGED) {
                //check bottle type
                if (this.currentBottle.type == Bottle.Type.B1) {
                    //change state of bottle to ingodown
                    this.currentBottle.changeState(Bottle.State.INGODOWN);
                    //add bottle to godown B1 buffer
                    this.godownB1.addNewBottle(this.currentBottle, Integer.MAX_VALUE);
                    //empty packaging unit and update state
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                } else {
                    //change state of bottle to ingodown
                    this.currentBottle.changeState(Bottle.State.INGODOWN);
                    //add bottle to godown B2 buffer
                    this.godownB2.addNewBottle(this.currentBottle, Integer.MAX_VALUE);
                    //empty packaging unit and update state
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                }
            }

            //change state of bottle to sealed
            this.currentBottle.changeState(Bottle.State.SEALED);
            //check sealing bottle type
            if (this.currentBottle.type == Bottle.Type.B1) {
                //check if B1 packaging buffer is full
                if (this.packagedB1.addNewBottle(this.currentBottle, 2)) {
                    //empty sealing unit and update state
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                } else {
                    //wait for B1 packaging buffer to remove a bottle
                    return packNextWakeupTime;
                }
            } else {
                //check if B2 packaging buffer is full
                if (this.packagedB2.addNewBottle(this.currentBottle, 3)) {
                    //empty packaging unit and update state
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                } else {
                    //wait for B2 packaging buffer to remove a bottle
                    return packNextWakeupTime;
                }
            }
        } else {
            //wait for sealing buffer to remove a bottle
            this.getNewBottle();
            //check if there is no bottle available in any buffer
            if (this.currentBottle == null) {
                // set next wake up time same as packaging unit
                return packNextWakeupTime > currentTime ? packNextWakeupTime : currentTime;
            } else {
                // set state equal to sealing
                this.state = State.SEALING;
                return currentTime;
            }
        }
    }

}
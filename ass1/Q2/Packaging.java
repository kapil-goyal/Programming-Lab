import java.util.*;

//handles the packaging operation
class Packaging {
    //possible states of packaging unitkaging operation
    enum State {
        EMPTY,
        PACKAGING,
        WAITING
    }

    //state of packaging unit
    State state;
    //priority for taking bottle from buffer
    boolean takeB1packed;
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

    //constructor for Packaging class
    public Packaging(UnfinishedTray unfinishedTray,
                     Buffer packagedB1,
                     Buffer packagedB2,
                     Buffer sealed,
                     Buffer godownB1,
                     Buffer godownB2) {
        this.state = State.EMPTY;
        this.takeB1packed = true;
        this.currentBottle = null;
        this.unfinishedTray = unfinishedTray;
        this.packagedB1 = packagedB1;
        this.packagedB2 = packagedB2;
        this.sealed = sealed;
        this.godownB1 = godownB1;
        this.godownB2 = godownB2;
    }

    //method to obtain new bottle when packaging unit is empty
    public void getNewBottle() {
        //check priority of bottle
        if (takeB1packed) {
            //obtain bottle from B1 sealed buffer
            currentBottle = this.packagedB1.getNewBottle();
            if (currentBottle != null) {
                takeB1packed = !takeB1packed;
            } else {
                currentBottle = this.packagedB2.getNewBottle();
            }
        } else {
            //obtain bottle from B2 sealed buffer
            currentBottle = this.packagedB2.getNewBottle();
            if (currentBottle != null) {
                takeB1packed = !takeB1packed;
            } else {
                currentBottle = this.packagedB1.getNewBottle();
            }
        }
        //obtain new bottle from unfinished tray
        if (currentBottle == null) {
            currentBottle = unfinishedTray.getNewBottleToPack();
        }
    }

    //handles the processing of the packaging unit
    public int Process(int currentTime, int sealNextWakeupTime) {
        //check if packaging has started
        if (this.state == State.PACKAGING) {
            //change state to waiting and return time of packaging
            this.state = State.WAITING;
            return currentTime + 2;
        }
        //check if packaging has finished
        else if (this.state == State.WAITING) {
            //check if bottle was already sealed
            if (this.currentBottle.state == Bottle.State.SEALED) {
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
            //change state of bottle to packaged
            this.currentBottle.changeState(Bottle.State.PACKAGED);
            //check if sealing buffer is full
            if (this.sealed.addNewBottle(this.currentBottle, 2)) {
                //empty packaging unit and update state
                this.currentBottle = null;
                this.state = State.EMPTY;
                return currentTime;
            } else {
                //wait for sealing buffer to remove a bottle
                return sealNextWakeupTime;
            }
        } else {
            //get new bottle if unit is empty
            this.getNewBottle();

            // if there is no bottle available in any buffer
            if (this.currentBottle == null) {
                // set next wake up time same as sealing unit
                return sealNextWakeupTime > currentTime ? sealNextWakeupTime : currentTime;
            } else {
                // set state equal to packaging
                this.state = State.PACKAGING;
                return currentTime;
            }
        }
    }

}

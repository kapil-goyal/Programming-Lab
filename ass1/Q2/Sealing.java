import java.util.*;

class Sealing {
    enum State{
        EMPTY,
        SEALING,
        WAITING
    }

    State state;
    Bottle currentBottle;
    UnfinishedTray unfinishedTray;
    Buffer packagedB1;
    Buffer packagedB2;
    Buffer sealed;
    Buffer godownB1;
    Buffer godownB2;

    public Sealing(UnfinishedTray unfinishedTray, 
                    Buffer packagedB1, 
                    Buffer packagedB2, 
                    Buffer sealed, 
                    Buffer godownB1, 
                    Buffer godownB2) 
    {
        this.state = State.EMPTY;
        this.currentBottle = null;
        this.unfinishedTray = unfinishedTray;
        this.packagedB1 = packagedB1;
        this.packagedB2 = packagedB2;
        this.sealed = sealed;
        this.godownB1 = godownB1;
        this.godownB2 = godownB2;
    }

    public void getNewBottle() {
        this.currentBottle = sealed.getNewBottle();
        if (this.currentBottle == null) {
            currentBottle = unfinishedTray.getNewBottleToSeal();
        }
    }

    public int Process(int currentTime, int packNextWakeupTime){
        if(this.state == State.SEALING)  {
            this.state = State.WAITING;
            return  currentTime + 3;
        }
        else if (this.state == State.WAITING) {
            if (this.currentBottle.state == Bottle.State.PACKAGED) {
                if (this.currentBottle.type == Bottle.Type.B1) {
                    this.currentBottle.changeState(Bottle.State.INGODOWN);
                    this.godownB1.addNewBottle(this.currentBottle, Integer.MAX_VALUE);
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                }
                else {
                    this.currentBottle.changeState(Bottle.State.INGODOWN);
                    this.godownB2.addNewBottle(this.currentBottle, Integer.MAX_VALUE);
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                }
            }

            this.currentBottle.changeState(Bottle.State.SEALED);

            if (this.currentBottle.type == Bottle.Type.B1) {
                if (this.packagedB1.addNewBottle(this.currentBottle, 2)) {
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                }
                else {
                    return packNextWakeupTime;
                }
            }
            else {
                if (this.packagedB2.addNewBottle(this.currentBottle, 3)) {
                    this.currentBottle = null;
                    this.state = State.EMPTY;
                    return currentTime;
                }
                else {
                    return packNextWakeupTime;
                }
            }
        }
        else {
            this.getNewBottle();
            if (this.currentBottle == null) {
                return currentTime+1;
            }
            else {
                this.state = State.SEALING;
                return currentTime;
            }
        }        
    }
    
}
import java.util.*;

class Packaging {
    enum State{
        EMPTY,
        PACKAGING,
        WAITING
    }

    State state;
    boolean takeB1packed;
    Bottle currentBottle;
    UnfinishedTray unfinishedTray;
    Buffer packagedB1;
    Buffer packagedB2;
    Buffer sealed;
    Buffer godownB1;
    Buffer godownB2;

    public Packaging(UnfinishedTray unfinishedTray, 
                    Buffer packagedB1, 
                    Buffer packagedB2, 
                    Buffer sealed, 
                    Buffer godownB1, 
                    Buffer godownB2) 
    {
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

    public void getNewBottle() {
        if(takeB1packed){
            currentBottle = this.packagedB1.getNewBottle();
            if (currentBottle != null) {
                takeB1packed = !takeB1packed;
            }
            else {
                currentBottle = this.packagedB2.getNewBottle();
            }
        }
        else{
            currentBottle = this.packagedB2.getNewBottle();
            if (currentBottle != null) {
                takeB1packed = !takeB1packed;
            }
            else {
                currentBottle = this.packagedB1.getNewBottle();
            }
        }
        if (currentBottle == null){
            currentBottle = unfinishedTray.getNewBottleToPack();
        }
    }

    public int Process(int currentTime, int sealNextWakeupTime){
        if(this.state == State.PACKAGING)  {
            this.state = State.WAITING;
            return  currentTime + 2;
        }
        else if (this.state == State.WAITING) {
            if (this.currentBottle.state == Bottle.State.SEALED) {
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

            this.currentBottle.changeState(Bottle.State.PACKAGED);

            if (this.sealed.addNewBottle(this.currentBottle, 2)) {
                this.currentBottle = null;
                this.state = State.EMPTY;
                return currentTime;
            }
            else {
                return sealNextWakeupTime;
            }
        }
        else {
            this.getNewBottle();
            if (this.currentBottle == null) {
                return sealNextWakeupTime > currentTime ? sealNextWakeupTime : currentTime;
            }
            else {
                this.state = State.PACKAGING;
                return currentTime;
            }
        }        
    }
    
}
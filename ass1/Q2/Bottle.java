// stores details about a bottle and updates state of bottle
public class Bottle {
    //possible states of a bottle
    enum State {
        UNFINISHED,
        PACKAGED,
        SEALED,
        INGODOWN
    }

    //possible bottle types
    enum Type {
        B1,
        B2
    }

    //bottle type
    Type type;
    //state of bottle
    State state;

    //constructor for creating bottle object
    public Bottle(boolean isB1) {
        // initially state is unfinished
        this.state = State.UNFINISHED;
        //assign bottle type
        if (isB1) {
            this.type = Type.B1;
        } else {
            this.type = Type.B2;
        }
    }

    //update state of bottle
    public void changeState(State newState) {
        this.state = newState;
    }

}
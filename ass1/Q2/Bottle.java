public class Bottle {
    enum State {
        UNFINISHED,
        PACKAGED,
        SEALED,
        INGODOWN
    }

    enum Type {
        B1, 
        B2
    }

    Type type;
    State state;

    public Bottle(boolean isB1) {
        this.state = State.UNFINISHED;
        if (isB1) {
            this.type = Type.B1;
        }
        else {
            this.type = Type.B2;
        }
    }

    public void changeState(State newState) {
        this.state = newState;
    }

}
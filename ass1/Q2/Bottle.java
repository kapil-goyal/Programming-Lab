public class Bottle {
    enum State {
        UNFINISHED,
        INPACKAGING,
        SEALEDINPACKAGING,
        PACKAGED,
        INSEALING,
        PACKAGEDINSEALING,
        SEALED,
        INGODOWN
    }

    enum Type {
        B1,
        B2
    }

    Type type;
    State state;

    public Bottle(boolean isB1, boolean toPack) {
        if (isB1) {
            this.type = B1;
        }
        else {
            this.type = B2;
        }
        if (toPack) {
            this.state = INPACKAGING;
        }
        else {
            this.state = INSEALING;
        }
    } 
}
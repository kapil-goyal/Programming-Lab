class Vehicle {
    enum Direction {
        EastWest,
        WestEast,
        SouthWest,
        WestSouth,
        EastSouth,
        SouthEast
    }
    int id;
    long timeStamp;
    long remainingTime;
    boolean status;
    Direction direction;

    public Vehicle(int id, long currentTime, Direction direction) {
        this.id = id;
        this.timeStamp = currentTime;
        this.remainingTime = Integer.MAX_VALUE;
        this.status = false;
        this.direction = direction;
    }

    public void printVehicleStatus() {
        System.out.println(id + "\t" + direction + "\t" + status + "\t" + remainingTime);
    }
}
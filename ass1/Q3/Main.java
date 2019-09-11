import java.util.*;

class VehicleQueue {
    Queue<Vehicle> laneQueue;
    long nextAssignTime;
    int type;

    public VehicleQueue(int type, int nextAssignTime) {
        laneQueue = new LinkedList<Vehicle>();
        this.type = type;
        this.nextAssignTime = nextAssignTime;
    }

    public void addNewVehicle(Vehicle newVehicle, TrafficLights trafficLights) {
        laneQueue.add(newVehicle);
        trafficLights.processVehicleQueues();
    }

    public Vehicle getNewVehicle() {
        Vehicle newVehicle = null;
        if (laneQueue.size() > 0)
            newVehicle = laneQueue.remove();
        return newVehicle;
    }

    public void processVehicleQueue(long currentTime, Queue<Vehicle> processedQueue, TrafficLights trafficLights) {
        while (true) {
            Vehicle newVehicle = this.getNewVehicle();
            if (newVehicle == null)
                break;
            while (true) {
                long currentSlot = ((nextAssignTime / 60) % 3 + type) % 3;
                if (currentSlot == 0) {
                    long remainingTime = 60 * (nextAssignTime / 60 + 1) - nextAssignTime;
                    if (remainingTime >= 6) {
                        newVehicle.remainingTime = nextAssignTime - currentTime;
                        processedQueue.add(newVehicle);
                        nextAssignTime += 6;
                        break;
                    }
                    else {
                        nextAssignTime = 60 * (nextAssignTime / 60 + 3);
                    }
                }
                else if (currentSlot == 1) {
                    nextAssignTime = 60 * (nextAssignTime / 60 + 2);
                }
                else {
                    nextAssignTime = 60 * (nextAssignTime / 60 + 1);
                }
            }
        }
    }

    void print() {
        for (Vehicle eachVehicle : this.laneQueue) {
            eachVehicle.printVehicleStatus();
        }
    }
}

class TrafficLights extends TimerTask {

    int activeTrafficLight;
    long elapsedTime;
    long[] remainingTime = {0,0,0};
    VehicleQueue t1Queue;
    VehicleQueue t2Queue;
    VehicleQueue t3Queue;
    Queue<Vehicle> processedQueue;
    Traffic traffic;

    public TrafficLights(int activeTrafficLight, VehicleQueue t1Queue, VehicleQueue t2Queue, VehicleQueue t3Queue) {
        this.activeTrafficLight = activeTrafficLight;
        this.elapsedTime = 0;
        this.t1Queue = t1Queue;
        this.t2Queue = t2Queue;
        this.t3Queue = t3Queue;
        this.remainingTime[(activeTrafficLight-1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 2) % 3] = 120;   
        processedQueue = new LinkedList<Vehicle>();      
        traffic = new Traffic((activeTrafficLight-1 == 0), 
                            (activeTrafficLight-1 == 1), 
                            (activeTrafficLight-1 == 2), 
                            this.remainingTime[0], 
                            this.remainingTime[1], 
                            this.remainingTime[2]);
    }

    public void processVehicleQueues() {
        t1Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);
        t1Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);
        t1Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);
    }

    public void updateTrafficLight() {
        activeTrafficLight = activeTrafficLight % 3 + 1;
        this.remainingTime[(activeTrafficLight-1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 2) % 3] = 120;
    }

    public void updateVehiclesStatus() {
        for (Vehicle eachVehicle : this.processedQueue) {
            eachVehicle.remainingTime = 0 < eachVehicle.remainingTime-1 ? eachVehicle.remainingTime-1 : 0;
            if (eachVehicle.remainingTime == 0) {
                eachVehicle.status = true;
            }
        }
    }

    public void displayTrafficLight() {
        System.out.println("Traffic Light\tStatus\tTime");
        System.out.println("T1\t\t" + (activeTrafficLight-1 == 0) + "\t\t" + this.remainingTime[0]);
        System.out.println("T2\t\t" + (activeTrafficLight-1 == 1) + "\t\t" + this.remainingTime[1]);
        System.out.println("T3\t\t" + (activeTrafficLight-1 == 2) + "\t\t" + this.remainingTime[2]);
        traffic.updateTimer((activeTrafficLight-1 == 0), 
                            (activeTrafficLight-1 == 1), 
                            (activeTrafficLight-1 == 2), 
                            this.remainingTime[0], 
                            this.remainingTime[1], 
                            this.remainingTime[2]);
    }

    public void displayVehiclesStatus() {
        System.out.println("Vehicle\tDirection\tStatus\tRemaining Time");
        for (Vehicle eachVehicle : this.processedQueue) {
            eachVehicle.printVehicleStatus();
        }
    }

    public void run() {
        // System.out.println(this.elapsedTime);
        System.out.println("\f");
        // this.elapsedTime++;
        displayTrafficLight();
        displayVehiclesStatus();
        this.elapsedTime++;
        if (this.elapsedTime % 60 == 0 && this.elapsedTime > 0) {
            this.updateTrafficLight();
        }
        else {
            remainingTime[0]--;
            remainingTime[1]--;
            remainingTime[2]--;
        }      
        updateVehiclesStatus();
    }
}

class Mythread extends Thread {
    TrafficLights trafficLights;
    Timer timer;
    

    public Mythread(TrafficLights trafficLights) {
        this.trafficLights = trafficLights;
        timer = new Timer();
    }
    public void run() {
        timer.schedule(this.trafficLights, 0, 100);
    }
}

class Main {

    public static void main(String[] args) {
        int vehicleID = 1;
        Scanner input = new Scanner(System.in);
        
        VehicleQueue t1Queue = new VehicleQueue(0,0);
        VehicleQueue t2Queue = new VehicleQueue(1,60);
        VehicleQueue t3Queue = new VehicleQueue(2,120);
        TrafficLights trafficLights = new TrafficLights(3, t1Queue, t2Queue, t3Queue);

        // int x = 4;

        // while (x-- > 0) {
        //     String source = input.next();
        //     String destination = input.next();

            // System.out.println(source);
            // System.out.println(destination);

        //     if (source.equals("N") || destination.equals("N")) {
        //         System.out.println("Error: Invalid input");
        //     }
        //     else if (source.equals("S") && destination.equals("E")) {
        //         Vehicle newVehicle = new Vehicle(vehicleID++, (System.currentTimeMillis() / 1000), Vehicle.Direction.SouthEast);
        //         t1Queue.addNewVehicle(newVehicle, trafficLights);
        //     }
        //     else if (source.equals("W") && destination.equals("S")) {
        //         Vehicle newVehicle = new Vehicle(vehicleID++, (System.currentTimeMillis() / 1000), Vehicle.Direction.WestSouth);
        //         t2Queue.addNewVehicle(newVehicle, trafficLights);
        //     }
        //     else if (source.equals("E") && destination.equals("W")) {
        //         Vehicle newVehicle = new Vehicle(vehicleID++, (System.currentTimeMillis() / 1000), Vehicle.Direction.EastWest);
        //         t3Queue.addNewVehicle(newVehicle, trafficLights);
        //     }
        //     else {
        //         System.out.println("Invalid Input");
        //     }
        // }
        // // System.out.println(t1Queue);
        // // System.out.println(t2Queue);
        // // System.out.println(t3Queue);
        // t1Queue.print();
        // t2Queue.print();
        // t3Queue.print();
        
        Mythread tempthread = new Mythread(trafficLights);
        tempthread.start();
        // tempthread.join();
    }
}
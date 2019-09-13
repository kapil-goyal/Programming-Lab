// Import libraries
import java.util.*;
import java.util.concurrent.Semaphore;
import java.awt.event.*;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableModel;
import javax.swing.JTable;
import javax.swing.JScrollPane;
import java.awt.BorderLayout;
import java.awt.Color;

// Vehicle class: This class defines a vehicle and its attributes
class Vehicle {

    // Six directions in which vehicles can move
    enum Direction {
        EastWest,
        WestEast,
        SouthWest,
        WestSouth,
        EastSouth,
        SouthEast
    }

    // Stores id a vehicle
    int id;

    // Stores timestamp when vehicle arrives in a lane
    long timeStamp;

    // Time remaining to pass for a vehicle
    long remainingTime;

    // Status will be true if vehicle can pass otherwise it will be false
    boolean status;

    // Direction in which vehicle wants to move
    Direction direction;

    // Constructor for vehicle class
    public Vehicle(int id, long currentTime, Direction direction) {
        this.id = id;
        this.timeStamp = currentTime;
        this.remainingTime = Integer.MAX_VALUE;
        this.status = false;
        this.direction = direction;
    }

    // This method will print vehicle status (used for debugging)
    public void printVehicleStatus() {
        System.out.println(id + "\t" + direction + "\t" + status + "\t" + remainingTime);
    }
}

// VehicleQueue class defines queue of vehicles in a lane
class VehicleQueue {

    // Queue of Vehicles in a traffic lane (can be T1/T2/T3)
    Queue<Vehicle> laneQueue;

    // Next possible time, a new vehicle can pass the traffic queue
    long nextAssignTime;

    // 0 means T1, 1 means T2, 2 means T3
    int type;

    // Used for synchronization
    Semaphore semaphore;

    // Constructor of VehicleQueue Class
    public VehicleQueue(int type, int nextAssignTime) {

        // Initialize a new queue
        laneQueue = new LinkedList<Vehicle>();

        // Assign type
        this.type = type;

        // Set next assign time
        this.nextAssignTime = nextAssignTime;

        // Used semaphore to implement a simple lock
        this.semaphore = new Semaphore(1);
    }

    // This method is called whenever a new vehicle arrives in a traffic lane
    public void addNewVehicle(Vehicle newVehicle, TrafficLights trafficLights) {
        try {
            // Try to acquire semaphore
            semaphore.acquire();

            // After semaphore is acquired add new vehicle in the queue
            laneQueue.add(newVehicle);

            // Process this newly added vehicle and calculate its remaining time
            trafficLights.processVehicleQueues();

            // If remaining time of this vehicle is found to be zero, 
            // this vehicle can pass
            if (newVehicle.remainingTime == 0) {
                newVehicle.status = true;
            }

            // This function is called to display vehicle table in UI
            trafficLights.updateVehicleTableUI();

            // Release the acquired semaphore
            semaphore.release();
        } catch (Exception e) {

            // Catch exception if semaphore acquire throws an exception
            System.out.println("Error: Cannot add vehicle");
        }        
    }

    // This gets first vehicle from traffic lane and return its pointer.
    // If vehicle is not present then null is returned
    public Vehicle getNewVehicle() {
        Vehicle newVehicle = null;
        if (laneQueue.size() > 0)
            newVehicle = laneQueue.remove();
        return newVehicle;
    }

    // This method processes all vehicles present in the traffic lane and assign their remaining time
    public void processVehicleQueue(long currentTime, Queue<Vehicle> processedQueue, TrafficLights trafficLights) {
        // if next assignment time is lesser than current time,
        // set next assignment time = current time
        this.nextAssignTime = currentTime > this.nextAssignTime ? currentTime : this.nextAssignTime;

        // Keep iterating until queue is not empty
        while (true) {

            // get new vehicle from the lane
            Vehicle newVehicle = this.getNewVehicle();

            // if there is no new vehicle then break the loop
            if (newVehicle == null)
                break;

            // else
            // Keep iterating till vehicle is not assigned its remaining time
            while (true) {

                // current slot == 0 means it is in green phase
                // 1 means it is in red phase and will become green in next phase
                // 2 means it was green in previous phase 
                //   and will take 2 slots of 60 seconds to get into green phase again
                long currentSlot = ((nextAssignTime / 60) % 3 + 3 - type) % 3;

                // Green phase
                if (currentSlot == 0) {

                    // get remaining time of traffic light in green phase
                    long remainingTime = 60 * (nextAssignTime / 60 + 1) - nextAssignTime;

                    // if remaining time >= 6, a vehicle can pass
                    if (remainingTime >= 6) {

                        // set remaining time for new vehicle
                        newVehicle.remainingTime = nextAssignTime - currentTime;

                        // add this processed vehicle in processed queue
                        processedQueue.add(newVehicle);

                        // next possible assignment time a will can pass
                        nextAssignTime += 6;

                        // after assigning vehicle its remaining time, break the loop
                        break;
                    }

                    // if remaining time is not sufficient for vehicle to pass
                    else {
                        // change next possible assignment time as the start time of the next green phase
                        nextAssignTime = 60 * (nextAssignTime / 60 + 3);
                    }
                }

                // First red phase
                else if (currentSlot == 1) {
                    // change next possible assignment time as the start time of the next green phase
                    nextAssignTime = 60 * (nextAssignTime / 60 + 2);
                }

                // 2nd red phase
                else {
                    // change next possible assignment time as the start time of the next green phase
                    nextAssignTime = 60 * (nextAssignTime / 60 + 1);
                }
            }
        }
    }

    // Method to print each vehicle of the queue
    void print() {
        for (Vehicle eachVehicle : this.laneQueue) {
            eachVehicle.printVehicleStatus();
        }
    }
}

// This class defines traffic lights and vehicle simulation
// it is extended as TimerTask class since it will be used by
// a timer object to run simulation after every 1 second
class TrafficLights extends TimerTask {

    // traffic light in green phase
    // 1 ==>  T1, 2 ==> T2, 3 ==> T3
    int activeTrafficLight;

    // Seconds elapsed since the starting of the simulation
    long elapsedTime;

    // array which stores 
    // remaining time for each traffic light (either in Red phase or in Green phase)
    long[] remainingTime = {0,0,0};

    // Vehicles queue for traffic light T1
    VehicleQueue t1Queue;

    // Vehicles queue for traffic light T2
    VehicleQueue t2Queue;

    // Vehicles queue for traffic light T3
    VehicleQueue t3Queue;

    // Queue which stores all the vehicles that are processed 
    // and assigned remaining time.
    Queue<Vehicle> processedQueue;

    // Traffic table for UI
    JTable trafficTable;

    // Vehicle table for UI
    JTable vehicleTable;

    // Constructor for Traffic Light class
    public TrafficLights(int activeTrafficLight, VehicleQueue t1Queue, VehicleQueue t2Queue, VehicleQueue t3Queue, JTable trafficTable, JTable vehicleTable) {
        this.activeTrafficLight = activeTrafficLight;
        this.elapsedTime = 0;
        this.t1Queue = t1Queue;
        this.t2Queue = t2Queue;
        this.t3Queue = t3Queue;
        this.trafficTable = trafficTable;
        this.vehicleTable = vehicleTable;
        this.remainingTime[(activeTrafficLight-1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 2) % 3] = 120;   
        processedQueue = new LinkedList<Vehicle>();

        // This method is called to display traffic table in UI
        this.updateTableUI((activeTrafficLight-1 == 0), 
                            (activeTrafficLight-1 == 1), 
                            (activeTrafficLight-1 == 2), 
                            this.remainingTime[0], 
                            this.remainingTime[1], 
                            this.remainingTime[2]);

        // This method is called to display vehicles table in UI
        this.updateVehicleTableUI();
    }

    // This method is used to process vehicles in all traffic lanes
    public void processVehicleQueues() {

        // Process all vehicles in traffic lane for T1
        t1Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);

        // Process all vehicles in traffic lane for T1
        t2Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);

        // Process all vehicles in traffic lane for T1
        t3Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);
    }

    // This method is called when a traffic light changes from a green phase to red phase
    // Traffic lights are updated in round robin manner
    public void updateTrafficLight() {
        // set current active traffic light
        activeTrafficLight = activeTrafficLight % 3 + 1;

        // update remaining time for each traffic light
        this.remainingTime[(activeTrafficLight-1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 1) % 3] = 60;
        this.remainingTime[((activeTrafficLight-1) + 2) % 3] = 120;
    }

    // This method is called to update remaining time for each vehicle after every second
    public void updateVehiclesStatus() {
        // For each processed vehicle
        for (Vehicle eachVehicle : this.processedQueue) {
            // decrease its remaining time by one and if negative then make it zero
            eachVehicle.remainingTime = 0 < eachVehicle.remainingTime-1 ? eachVehicle.remainingTime-1 : 0;

            // if remaining time is zero then vehicle can pass the traffic lane
            // So set its status as true
            if (eachVehicle.remainingTime == 0) {
                eachVehicle.status = true;
            }
        }
    }

    // This method is called to update vehicle table UI after every second
    public void updateVehicleTableUI(){

        // Stores index of each vehicle in processed queue
        int i=0;

        // lastvehicle stores a vehicle pointer if a new vehicle is added in the processed queue
        Vehicle lastVehicle = new Vehicle(0,0,Vehicle.Direction.EastSouth);

        // for each vehicle in processed queue
        for(Vehicle item: this.processedQueue){

            // if a new vehicle is added in the processed queue, 
            // get last vehicle and stores its attributes and then break the loop
            if(i >= vehicleTable.getRowCount()){
                lastVehicle.id = item.id;
                lastVehicle.timeStamp = item.timeStamp;
                lastVehicle.direction = item.direction;
                lastVehicle.status = item.status;
                lastVehicle.remainingTime = item.remainingTime;
                break;
            }

            // if current status of vehicle is pass, set its value as pass in Table
            if(item.status)
                vehicleTable.setValueAt("Pass", i, 3);

            // else set its value as wait
            else
                vehicleTable.setValueAt("Wait", i, 3);
            
            // Update remaining time of the current vehicle
            vehicleTable.setValueAt(String.valueOf(item.remainingTime), i, 4);
            i++;    
        }

        // If a new vehicle was added, add a new row in Vehicle table UI
        if(this.processedQueue.size() > vehicleTable.getRowCount()){
            
            // Get the table model of vehicle table UI
            DefaultTableModel model = (DefaultTableModel) vehicleTable.getModel();

            // Set values of the last vehicle in newVechicle vector
            Vector<String> newVehicle = new Vector<String>();
            newVehicle.add(String.valueOf(lastVehicle.id));
            if(lastVehicle.direction == Vehicle.Direction.EastSouth){
                newVehicle.add("East");
                newVehicle.add("South");
            }
            else if(lastVehicle.direction == Vehicle.Direction.EastWest){
                newVehicle.add("East");
                newVehicle.add("West");
            }
            else if(lastVehicle.direction == Vehicle.Direction.SouthEast){
                newVehicle.add("South");
                newVehicle.add("East");
            }
            else if(lastVehicle.direction == Vehicle.Direction.SouthWest){
                newVehicle.add("South");
                newVehicle.add("West");
            }
            else if(lastVehicle.direction == Vehicle.Direction.WestEast){
                newVehicle.add("West");
                newVehicle.add("East");
            }
            else{
                newVehicle.add("West");
                newVehicle.add("South");
            }
            if(lastVehicle.status){
                newVehicle.add("Pass");
            }
            else
                newVehicle.add("Wait");

            // add new vehicle's remaining time
            newVehicle.add(String.valueOf(lastVehicle.remainingTime));

            // add new vehicle in the table model
            model.addRow(newVehicle);
        }
    }

    // This method is called to update traffic lights table UI after every second
    public void updateTableUI(boolean b1, boolean b2, boolean b3, long t1, long t2, long t3){

        // Set Red / Green Status for each traffic light
        this.trafficTable.setValueAt("Red", 0, 1);
        this.trafficTable.setValueAt("Red", 1, 1);
        this.trafficTable.setValueAt("Red", 2, 1);
        if(b1)
            this.trafficTable.setValueAt("Green", 0, 1);
        else if(b2)
            this.trafficTable.setValueAt("Green", 1, 1);
        else
            this.trafficTable.setValueAt("Green", 2, 1);
        
        // Set remaining time for each traffic light
        this.trafficTable.setValueAt(String.valueOf(t1), 0, 2);
        this.trafficTable.setValueAt(String.valueOf(t2), 1, 2);
        this.trafficTable.setValueAt(String.valueOf(t3), 2, 2);
    }

    // This method display traffic lights status
    public void displayTrafficLight() {
        // System.out.println("Traffic Light\tStatus\tTime");
        // System.out.println("T1\t\t" + (activeTrafficLight-1 == 0) + "\t\t" + this.remainingTime[0]);
        // System.out.println("T2\t\t" + (activeTrafficLight-1 == 1) + "\t\t" + this.remainingTime[1]);
        // System.out.println("T3\t\t" + (activeTrafficLight-1 == 2) + "\t\t" + this.remainingTime[2]);

        // This method is called to update traffic lights table UI
        this.updateTableUI((activeTrafficLight-1 == 0), 
                            (activeTrafficLight-1 == 1), 
                            (activeTrafficLight-1 == 2), 
                            this.remainingTime[0], 
                            this.remainingTime[1], 
                            this.remainingTime[2]);
    }

    // This method displays vehicles status
    public void displayVehiclesStatus() {
        // System.out.println("Vehicle\tDirection\tStatus\tRemaining Time");
        // for (Vehicle eachVehicle : this.processedQueue) {
        //     eachVehicle.printVehicleStatus();
        // }

        // This method is called to update Vehicle table UI
        this.updateVehicleTableUI();
    }

    // This method is called by timer object after every one second
    public void run() {
        // Display traffic light status
        displayTrafficLight();

        // Display vehicle status
        displayVehiclesStatus();

        // increment elapsed time
        this.elapsedTime++;

        // if another 60 seconds are passed, change phase of each traffic light
        if (this.elapsedTime % 60 == 0 && this.elapsedTime > 0) {
            this.updateTrafficLight();
        }
        // else decrease remaining time of each traffic light
        else {
            remainingTime[0]--;
            remainingTime[1]--;
            remainingTime[2]--;
        }      

        // update vehicle status for each vehicle
        updateVehiclesStatus();
    }
}

// This new thread class implements a thread
// to simulate traffic light and vehicle system
// after every one second
class Mythread extends Thread {
    TrafficLights trafficLights;
    Timer timer;
    
    // constructor for MyThread class
    public Mythread(TrafficLights trafficLights) {
        this.trafficLights = trafficLights;
        timer = new Timer();
    }

    public void run() {
        // schedule traffic lights after every 1 second
        timer.schedule(this.trafficLights, 0, 1000);
    }
}

class Main {

    // Vehicle id is initialized as 1
    static int vehicleID = 1;

    // Main function
    public static void main(String[] args) {
        
        // Declaration of the three traffic queues
        VehicleQueue t1Queue = new VehicleQueue(0,0);
        VehicleQueue t2Queue = new VehicleQueue(1,60);
        VehicleQueue t3Queue = new VehicleQueue(2,120);
        
        // UI elements
        JTextField directionFrom = new JTextField(10);;
        JTextField directionTo = new JTextField(10);
        JButton submit = new JButton("Submit");
        JFrame frame = new JFrame();
        JPanel trafficPanel = new JPanel();
        JPanel vehiclePanel = new JPanel();

        // Traffic table UI element
        String[] cols = {"Traffic Light", "Status", "Time"};
        String[][] rowData = {{"T1","Green","60"},{"T2","Red","60"},{"T3","Red","120"}};
        JTable trafficTable = new JTable(rowData, cols);
        trafficPanel.setLayout(new BorderLayout());
        trafficPanel.add(trafficTable, BorderLayout.CENTER);
        trafficPanel.add(trafficTable.getTableHeader(), BorderLayout.NORTH);

        // Vehicle table UI element
        String[] colsVehicle = {"Vehicle", "Source", "Destination", "Status", "Remaining Time"};
        DefaultTableModel vehicleModel = new DefaultTableModel();
        vehicleModel.setColumnIdentifiers(colsVehicle);
        JTable vehicleTable = new JTable(vehicleModel);     
        JScrollPane scrollPane = new JScrollPane(vehicleTable);
        vehiclePanel.add(scrollPane, BorderLayout.CENTER);

        // Set UI bounds
        frame.setSize( 520, 700 );
        directionFrom.setBounds(70, 50, 100, 20);
        directionTo.setBounds(190, 50, 100, 20);
        submit.setBounds(310, 50, 100, 20);
        trafficPanel.setBounds(70,100,340,70);
        vehiclePanel.setBounds(-50,200,600,500);

        // Add UI elements in UI frame
        frame.add(vehiclePanel); 
        frame.add(trafficPanel);
        frame.add(directionFrom);
        frame.add(directionTo);
        frame.add(submit);

        frame.setLayout(null);
        frame.setVisible(true);
        
        // Initialize traffic light object
        TrafficLights trafficLights = new TrafficLights(1, t1Queue, t2Queue, t3Queue, trafficTable, vehicleTable);

        // Submit listener
        // action performed function is called whenever submit button is pressed
        submit.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent e){
                String s = e.getActionCommand(); 
                if (s.equals("Submit")) { 

                    // Store source and destination direction
                    String source = directionFrom.getText();
                    String destination = directionTo.getText();        

                    // Add a vehicle in T1 traffic light lane
                    if (source.equals("S") && destination.equals("E")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.SouthEast);
                        t1Queue.addNewVehicle(newVehicle, trafficLights);
                    }
                    // Add a vehicle in processed queue
                    else if (source.equals("E") && destination.equals("S")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.EastSouth);
                        newVehicle.remainingTime = 0;
                        newVehicle.status = true;
                        trafficLights.processedQueue.add(newVehicle);
                        trafficLights.updateVehicleTableUI();
                    }
                    // Add a vehicle in T1 traffic light lane
                    else if (source.equals("W") && destination.equals("S")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.WestSouth);
                        t2Queue.addNewVehicle(newVehicle, trafficLights);
                    }
                    // Add a vehicle in processed queue
                    else if (source.equals("S") && destination.equals("W")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.SouthWest);
                        newVehicle.remainingTime = 0;
                        newVehicle.status = true;
                        trafficLights.processedQueue.add(newVehicle);
                        trafficLights.updateVehicleTableUI();
                    }
                    // Add a vehicle in T1 traffic light lane
                    else if (source.equals("E") && destination.equals("W")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.EastWest);
                        t3Queue.addNewVehicle(newVehicle, trafficLights);
                    }
                    // Add a vehicle in processed queue
                    else if (source.equals("W") && destination.equals("E")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.WestEast);
                        newVehicle.remainingTime = 0;
                        newVehicle.status = true;
                        trafficLights.processedQueue.add(newVehicle);
                        trafficLights.updateVehicleTableUI();
                    }
                    // All other cases are invalid input
                    else {
                        System.out.println("Invalid Input");
                    }
                }
            } 
        });

        // Make a new thread which simulates traffic light system and start it
        Mythread simulateTrafficLight = new Mythread(trafficLights);
        simulateTrafficLight.start();
    }
}
import java.util.*;
import java.awt.event.*;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableModel;

// import Vehicle.Direction;

import javax.swing.JTable;
import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JScrollPane;

class VehicleQueue {
    Queue<Vehicle> laneQueue;
    long nextAssignTime;
    int type;

    public VehicleQueue(int type, int nextAssignTime) {
        laneQueue = new LinkedList<Vehicle>();
        this.type = type;
        this.nextAssignTime = nextAssignTime;
    }

    public synchronized void addNewVehicle(Vehicle newVehicle, TrafficLights trafficLights) {
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
                long currentSlot = ((nextAssignTime / 60) % 3 + 3 - type) % 3;
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
    JTable trafficTable;
    JTable vehicleTable;

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
        this.updateTableUI((activeTrafficLight-1 == 0), 
                            (activeTrafficLight-1 == 1), 
                            (activeTrafficLight-1 == 2), 
                            this.remainingTime[0], 
                            this.remainingTime[1], 
                            this.remainingTime[2]);
        this.updateVehicles();
    }

    public void processVehicleQueues() {
        t1Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);
        t2Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);
        t3Queue.processVehicleQueue(this.elapsedTime, processedQueue, this);
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

    public void updateVehicles(){
        int i=0;
        // System.out.print("-----------------------------------");
        Vehicle lastVehicle = new Vehicle(0,0,Vehicle.Direction.EastSouth);
        for(Vehicle item: this.processedQueue){
            if(i >= vehicleTable.getRowCount()){
                lastVehicle.id = item.id;
                lastVehicle.timeStamp = item.timeStamp;
                lastVehicle.direction = item.direction;
                lastVehicle.status = item.status;
                lastVehicle.remainingTime = item.remainingTime;
                break;
            }
            if(item.status)
                vehicleTable.setValueAt("Pass", i, 3);
            else
                vehicleTable.setValueAt("Wait", i, 3);
            vehicleTable.setValueAt(String.valueOf(item.remainingTime), i, 4);
            i++;    
        }
        if(this.processedQueue.size() > vehicleTable.getRowCount()){
            DefaultTableModel model = (DefaultTableModel) vehicleTable.getModel();
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
            newVehicle.add(String.valueOf(lastVehicle.remainingTime));

            model.addRow(newVehicle);
        }
        // System.out.println("----------------------------------------" + vehicleTable.getRowCount() + "++++++++++++" + this.processedQueue.size());
        
    }

    public void updateTableUI(boolean b1, boolean b2, boolean b3, long t1, long t2, long t3){
        this.trafficTable.setValueAt("Red", 0, 1);
        this.trafficTable.setValueAt("Red", 1, 1);
        this.trafficTable.setValueAt("Red", 2, 1);
        if(b1)
            this.trafficTable.setValueAt("Green", 0, 1);
        else if(b2)
            this.trafficTable.setValueAt("Green", 1, 1);
        else
            this.trafficTable.setValueAt("Green", 2, 1);
        
        this.trafficTable.setValueAt(String.valueOf(t1), 0, 2);
        this.trafficTable.setValueAt(String.valueOf(t2), 1, 2);
        this.trafficTable.setValueAt(String.valueOf(t3), 2, 2);
    }

    public void displayTrafficLight() {
        System.out.println("Traffic Light\tStatus\tTime");
        System.out.println("T1\t\t" + (activeTrafficLight-1 == 0) + "\t\t" + this.remainingTime[0]);
        System.out.println("T2\t\t" + (activeTrafficLight-1 == 1) + "\t\t" + this.remainingTime[1]);
        System.out.println("T3\t\t" + (activeTrafficLight-1 == 2) + "\t\t" + this.remainingTime[2]);
        this.updateTableUI((activeTrafficLight-1 == 0), 
                            (activeTrafficLight-1 == 1), 
                            (activeTrafficLight-1 == 2), 
                            this.remainingTime[0], 
                            this.remainingTime[1], 
                            this.remainingTime[2]);
        this.updateVehicles();
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
        timer.schedule(this.trafficLights, 0, 500);
    }
}

class Main {
    static int vehicleID = 1;
    public static void main(String[] args) {
        
        Scanner input = new Scanner(System.in);
        
        VehicleQueue t1Queue = new VehicleQueue(0,0);
        VehicleQueue t2Queue = new VehicleQueue(1,60);
        VehicleQueue t3Queue = new VehicleQueue(2,120);
        
        JTextField directionFrom = new JTextField(10);;
        JTextField directionTo = new JTextField(10);
        JButton submit = new JButton("Submit");
        JFrame frame = new JFrame();
        JPanel panel = new JPanel();
        JPanel p1 = new JPanel();

        String[] cols = {"Traffic Light", "Status", "Time"};
        String[][] rowData = {{"T1","Green","60"},{"T2","Red","60"},{"T3","Red","120"}};
        JTable trafficTable = new JTable(rowData, cols);

        String[] colsVehicle = {"Vehicle", "Source", "Destination", "Status", "Remaining Time"};
        // String[][] vehicleData = {};
        DefaultTableModel vehicleModel = new DefaultTableModel();
        vehicleModel.setColumnIdentifiers(colsVehicle);
        JTable vehicleTable = new JTable(vehicleModel);

        frame.setSize( 460, 700 );

        panel.setLayout(new BorderLayout());
        panel.add(trafficTable, BorderLayout.CENTER);
        panel.add(trafficTable.getTableHeader(), BorderLayout.NORTH);
        panel.setBounds(50,100,340,70);
        frame.add(panel);

        JScrollPane scrollPane = new JScrollPane(vehicleTable);
        // scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);  
        JPanel p = new JPanel();
        p.add(scrollPane, BorderLayout.CENTER);

        directionFrom.setBounds(50, 50, 100, 20);
        directionTo.setBounds(170, 50, 100, 20);
        submit.setBounds(290, 50, 100, 20);
        frame.add(directionFrom);
        frame.add(directionTo);
        frame.add(submit);


        // p1.setLayout(new BorderLayout());
        // p1.add(vehicleTable, BorderLayout.CENTER);
        // p1.add(vehicleTable.getTableHeader(), BorderLayout.NORTH);
        p.setBounds(50,200,460,300);
        // frame.add(p1);
        frame.add(p);  

        frame.setLayout(null);
        frame.setVisible(true);
        

        TrafficLights trafficLights = new TrafficLights(1, t1Queue, t2Queue, t3Queue, trafficTable, vehicleTable);
        // NewVehicle vehicleUI = new NewVehicle(frame, t1Queue, t2Queue, t3Queue, trafficLights);

        submit.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent e){
                String s = e.getActionCommand(); 
                if (s.equals("Submit")) { 
                    String source = directionFrom.getText();
                    String destination = directionTo.getText();        
                    directionFrom.setText("");
                    directionTo.setText(""); 
                    System.out.println(source + " "+ destination);   
                    if (source.equals("N") || destination.equals("N")) {
                        System.out.println("Error: Invalid input");
                    }
                    else if (source.equals("S") && destination.equals("E")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.SouthEast);
                        t1Queue.addNewVehicle(newVehicle, trafficLights);
                    }
                    else if (source.equals("W") && destination.equals("S")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.WestSouth);
                        t2Queue.addNewVehicle(newVehicle, trafficLights);
                    }
                    else if (source.equals("E") && destination.equals("W")) {
                        Vehicle newVehicle = new Vehicle(vehicleID++, trafficLights.elapsedTime, Vehicle.Direction.EastWest);
                        t3Queue.addNewVehicle(newVehicle, trafficLights);
                    }
                    else {
                        System.out.println("Invalid Input");
                    }
                }
            } 
        });

        // Panel pane = new JPanel();
        // lightsTable.setBounds(30, 40, 200, 300); 
        // pane.setBounds(0,0,400,400);
        // pane.add(lightsTable);
        
        // frame.add(pane);
        
        

        // int x = 4;

        // while (x-- > 0) {
        //     String source = input.next();
        //     String destination = input.next();

        //     System.out.println(source);
        //     System.out.println(destination);

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
        // while (true) {
        //     ;
        // }
        // System.out.println(t1Queue);
        // System.out.println(t2Queue);
        // System.out.println(t3Queue);
        // t1Queue.print();
        // t2Queue.print();
        // t3Queue.print();
        // for (Vehicle eachVehicle : trafficLights.processedQueue) {
        //     eachVehicle.printVehicleStatus();
        // }
        
        Mythread tempthread = new Mythread(trafficLights);
        tempthread.start();
        // tempthread.join();
    }
}
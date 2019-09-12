import javax.swing.*;
import java.util.*;
import java.awt.event.*;

class NewVehicle{
    JTextField directionFrom;
    JTextField directionTo;
    JButton submit;
    JFrame frame;
    VehicleQueue t1Queue;
    VehicleQueue t2Queue;
    VehicleQueue t3Queue;
    TrafficLights trafficLights;
    int vehicleID;

    public NewVehicle(JFrame frame, 
                      VehicleQueue t1Queue, 
                      VehicleQueue t2Queue, 
                      VehicleQueue t3Queue, 
                      TrafficLights trafficLights) {
        this.t1Queue = t1Queue;
        this.t2Queue = t2Queue;
        this.t3Queue = t3Queue;
        this.vehicleID = 1;
        this.frame = frame;
        this.trafficLights = trafficLights;

        directionFrom = new JTextField(20);
        directionTo = new JTextField(20);
        submit = new JButton("Submit");
        submit.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent e){
                String s = e.getActionCommand(); 
                if (s.equals("Submit")) { 
                    String source = directionFrom.getText();
                    String destination = directionTo.getText();        
                    directionFrom.setText("");
                    directionTo.setText(""); 
                    // System.out.println(from + " "+ to);   
                    // if (source.equals("N") || destination.equals("N")) {
                    //     System.out.println("Error: Invalid input");
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
                } 
            }
        });
        JPanel p = new JPanel();
        p.add(directionFrom);
        p.add(directionTo);
        JPanel p1 = new JPanel();
        JPanel p2 = new JPanel();
        p1.add(submit);
        p.setBounds(500,500,900,600);
        p1.setBounds(800, 500, 1200, 1200);
        p2.setBounds(1300, 1300, 1300, 1300);
        this.frame.add(p);
        this.frame.add(p1);
        this.frame.add(p2);
        this.frame.show();

        
    }
}
import javax.swing.*;
import java.util.*;
import javax.swing.table.*;
import java.awt.event.*;

class Traffic{
    DefaultTableModel lightsTableModel;
    JTable trafficTable;
    JFrame frame;
    static final long serialVersionUID = 5;
    

    public Traffic(JFrame frame, boolean b1, boolean b2, boolean b3, long t1, long t2, long t3){
        this.frame = frame;
        String[] cols = {"Traffic Light", "Status", "Time"};
        String[][] rowData = {{"T1","Green","60"},{"T2","Red","60"},{"T3","Red","120"}};
                
        lightsTableModel = new DefaultTableModel();
        lightsTableModel.setColumnIdentifiers(cols);
        for(int j=0;j<rowData.length;j++){
            Vector<String> temp = new Vector<String>();
            for(int k=0;k<rowData[j].length;k++)
                temp.addElement(rowData[j][k]);
            lightsTableModel.addRow(temp);
        }

        trafficTable = new JTable(rowData, cols);        

        // JPanel pane = new JPanel();
        // lightsTable.setBounds(100, 100, 1, 1); 
        // pane.setBounds(1000,1000,400,400);
        // pane.add(lightsTable);
        trafficTable.setBounds(30,40,200,300);   
        frame.add(trafficTable);
        updateTimer(b1, b2, b3, t1, t2, t3);
        frame.setVisible(true);
    }

    public void updateTimer(boolean b1, boolean b2, boolean b3, long t1, long t2, long t3){
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

    
}

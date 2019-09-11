import javax.swing.*;
import java.util.*;
import javax.swing.table.*;

class Traffic{
    DefaultTableModel lightsTableModel;
    JTable lightsTable;
    JFrame frame;
    static final long serialVersionUID = 5;

    public Traffic(){
        JFrame frame = new JFrame();
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

        lightsTable = new JTable(lightsTableModel);
        JPanel pane = new JPanel();
        // lightsTable.print();
        // for(int i=0;i<rowData.length;i++){
        //     for(int j=0;j<cols.length;j++)
        //         System.out.println(lightsTable.getModel().getValueAt(i, j)+" ");
        // }
        lightsTable.setBounds(30, 40, 200, 300); 
        pane.setBounds(0,0,400,400);
        pane.add(lightsTable);
        // JLabel l = new JLabel("nothing entered"); 
        // pane.add(l);
        frame.add(pane);
        frame.setSize( 500, 500 );
        // frame.show();
        frame.setVisible(true);
    }

    public void updateTimer(boolean b1, boolean b2, boolean b3, long t1, long t2, long t3){
        this.lightsTableModel.setValueAt("Red", 0, 1);
        this.lightsTableModel.setValueAt("Red", 1, 1);
        this.lightsTableModel.setValueAt("Red", 2, 1);
        if(b1)
            this.lightsTableModel.setValueAt("Green", 0, 1);
        else if(b2)
            this.lightsTableModel.setValueAt("Green", 1, 1);
        else
            this.lightsTableModel.setValueAt("Green", 2, 1);
        
        this.lightsTableModel.setValueAt(String.valueOf(t1), 0, 2);
        this.lightsTableModel.setValueAt(String.valueOf(t2), 1, 2);
        this.lightsTableModel.setValueAt(String.valueOf(t3), 2, 2);
        // this.frame.setVisible(true);
    }

    
}

// class Ui{
//     public static void main(String[] args){
//         Traffic traffic = new Traffic();
//     }
// }
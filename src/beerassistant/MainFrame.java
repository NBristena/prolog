/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package beerassistant;

import java.awt.FlowLayout;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JLabel;

/**
 *
 * @author Zorro Andrei
 */
public class MainFrame extends javax.swing.JFrame {

// <editor-fold defaultstate="collapsed" desc="Componente">
    ConnectionSicstus conn;
    QuestionFrame qFrame;
    public static boolean AFISAT_SOLUTII = false;
    public static boolean Reset = false;
    public static boolean Multiple_Choices=false;
// </editor-fold>
// <editor-fold defaultstate="collapsed" desc="Constructor"> 

    /**
     * Creates new form MainFrame
     */
    public MainFrame(String titlu) {
        super(titlu);
        qFrame=new QuestionFrame();
        initComponents();
    }
// </editor-fold>  
    
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        startFrame = new javax.swing.JPanel();
        start = new javax.swing.JButton();
        welcome = new javax.swing.JLabel();
        stats = new javax.swing.JButton();
        exit = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        debug = new javax.swing.JTextArea();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        startFrame.setPreferredSize(new java.awt.Dimension(240, 170));

        start.setText("Start");
        start.setActionCommand("start");
        start.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                startActionPerformed(evt);
            }
        });

        welcome.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        welcome.setText("Bine ai venit la Beer Assistent!");
        welcome.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        stats.setText("Statistici");

        exit.setText("Exit");
        exit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exitActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout startFrameLayout = new javax.swing.GroupLayout(startFrame);
        startFrame.setLayout(startFrameLayout);
        startFrameLayout.setHorizontalGroup(
            startFrameLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(startFrameLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(welcome, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
            .addGroup(startFrameLayout.createSequentialGroup()
                .addGap(85, 85, 85)
                .addGroup(startFrameLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(exit, javax.swing.GroupLayout.PREFERRED_SIZE, 69, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(start, javax.swing.GroupLayout.PREFERRED_SIZE, 69, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(86, Short.MAX_VALUE))
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, startFrameLayout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(stats)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        startFrameLayout.setVerticalGroup(
            startFrameLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(startFrameLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(welcome, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(start)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(stats)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(exit)
                .addContainerGap(25, Short.MAX_VALUE))
        );

        debug.setColumns(20);
        debug.setRows(5);
        jScrollPane1.setViewportView(debug);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(80, 80, 80)
                        .addComponent(startFrame, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(96, 96, 96)
                        .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(80, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(startFrame, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(46, 46, 46)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(78, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void exitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exitActionPerformed
        // TODO add your handling code here:
        try {
            conn.sender.trimiteMesajSicstus("exit");
        } catch (InterruptedException ex) {
            Logger.getLogger(MainFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
        System.exit(0);
    }//GEN-LAST:event_exitActionPerformed

    private void startActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_startActionPerformed
        // TODO add your handling code here:
        
        MainFrame.AFISAT_SOLUTII=false;
        
        String dir=System.getProperty("user.dir");
        dir=dir.replace("\\", "/");
        System.out.println(dir);
        try {
            conn.sender.trimiteMesajSicstus("director('"+dir+"')");
            conn.sender.trimiteMesajSicstus("incarca");
        
        } catch (InterruptedException ex) {
            Logger.getLogger(MainFrame.class.getName()).log(Level.SEVERE, null, ex);
        }

        this.remove(this.start);
        this.remove(this.exit);
        this.remove(this.stats);
        this.remove(this.startFrame);
        this.remove(this.welcome);
        
        this.setLayout(new FlowLayout());
        this.add(this.qFrame);
        this.qFrame.paint(null);
        this.qFrame.revalidate();
        this.repaint();
        this.revalidate();
        try {
            conn.sender.trimiteMesajSicstus("comanda(consulta)");
            
        
        } catch (InterruptedException ex) {
            Logger.getLogger(MainFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    }//GEN-LAST:event_startActionPerformed

// <editor-fold defaultstate="collapsed" desc="Event Butoane Dinamice">
    private void optionActionPerformed(java.awt.event.ActionEvent evt) {                                           
       
       
        String raspuns= ((JButton)(evt.getSource())).getText();
        try {
            conn.sender.trimiteSirSicstus(raspuns);
            
        
        } catch (InterruptedException ex) {
            Logger.getLogger(MainFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    } 
// </editor-fold >
    
// <editor-fold defaultstate="collapsed" desc="Metodele mele"> 
    public javax.swing.JTextArea getDebug(){
        return debug;
    }
    
    public void setConnection(ConnectionSicstus _conn){
        conn = _conn;
    }
    
    public void setQuestion(String question){
        this.qFrame.questionLabel.setText(
                "<html><body style='width:100%; align-content:center; dysplay:flex;'>"+question+"</html>"
        );
        this.qFrame.repaint();
    }
    
    public void setOptions(String options){
        
        this.qFrame.optionsPanel.removeAll();
        //System.out.println("incarc optiuni");
        this.qFrame.optionsPanel.setLayout(new FlowLayout());
        options=options.trim();
        options=options.substring(1,options.length()-1);
        options=options.trim();
        String[] vect_optiuni=options.split(" ");
        for(int i=0;i<vect_optiuni.length;i++)
        {
            //CREEZ BUTOANE PT OPTIUNI
            JButton b=new JButton(vect_optiuni[i]);
            b.addActionListener(
                    new java.awt.event.ActionListener() {
                    public void actionPerformed(java.awt.event.ActionEvent evt) 
                    {
                    optionActionPerformed(evt);
                    }
            });
            
            //ADAUG BUTOANELE
            this.qFrame.optionsPanel.add(b);
        }
        this.qFrame.optionsPanel.repaint();
        this.qFrame.optionsPanel.revalidate();
    }  

     public void setSolution(String solutie){
        if(!MainFrame.AFISAT_SOLUTII)
        {
            this.qFrame.removeAll();
            this.qFrame.setLayout(new FlowLayout());
            MainFrame.AFISAT_SOLUTII=true;
        }

        JLabel jsol=new JLabel(solutie);
        this.qFrame.add(jsol);
       
        this.qFrame.repaint();
        this.qFrame.revalidate();
        //this.revalidate();
    } 
// </editor-fold>  
    
    
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html 
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(MainFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new MainFrame("").setVisible(true);
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextArea debug;
    private javax.swing.JButton exit;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton start;
    private javax.swing.JPanel startFrame;
    private javax.swing.JButton stats;
    private javax.swing.JLabel welcome;
    // End of variables declaration//GEN-END:variables
}

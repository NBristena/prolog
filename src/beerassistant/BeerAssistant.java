/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package beerassistant;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Zorro Andrei
 */
public class BeerAssistant {

// <editor-fold defaultstate="collapsed" desc="Componente">    
    
    static final int PORT=5007;
// </editor-fold>    
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        
        ConnectionSicstus conn;
        
        try {            
            final MainFrame frame=new MainFrame("Exemplu Interfata Prolog");
            System.out.println("Here i m");
            conn=new ConnectionSicstus(PORT,frame);
            frame.setConnection(conn);
            frame.setVisible(true);
            frame.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    try {
                        frame.conn.opresteProlog();                        
                        frame.conn.sender.gata=true;
                    } catch (InterruptedException ex) {
                        Logger.getLogger(BeerAssistant.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            });
            //String dir=System.getProperty("user.dir");
            //dir=dir.replace("\\", "/");
            //fereastra.conexiune.expeditor.trimiteMesajSicstus("director('"+dir+"')");
            /*try {
                 fereastra.conexiune.expeditor.trimiteMesajSicstus("salut");
                 System.out.println("ceva");
                } catch (InterruptedException ex) {
              Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
             }*/
        } catch (IOException ex) {
            Logger.getLogger(BeerAssistant.class.getName()).log(Level.SEVERE, null, ex);
            System.out.println("Eroare clasa initiala");
        } catch (InterruptedException ex) {
            Logger.getLogger(BeerAssistant.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
}

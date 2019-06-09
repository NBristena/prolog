/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package beerassistant;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
/**
 *
 * @author Zorro Andrei
 */
public class ConnectionSicstus {
   
// <editor-fold defaultstate="collapsed" desc="Componente"> 
    
    //path-ul trebuie schimbat !!!!!!!!!
    final String pathSicstus="D:\\Programe\\Install\\SICSTUS PROLOG\\SICStus Prolog 4.0.2\\bin\\spwin.exe";
    final String nume_fisier="sist_exp_bere.pl";  
    final String scop="inceput.";
    
    Process procesSicstus;
    MessageSender sender;
    MessageReader reader;
    MainFrame frame;
    int port;
    
// </editor-fold>
    
// <editor-fold defaultstate="collapsed" desc="Constructor"> 
    
    public ConnectionSicstus(int _port, MainFrame _frame) 
            throws IOException, InterruptedException{
        
        InputStream processIs, processStreamErr;
        port=_port;
        frame=_frame;
        
        ServerSocket servs = new ServerSocket(port);
        //Socket sock_s=servs.accept();
        
        reader = new MessageReader(this,servs);
        reader.start();
        
        sender = new MessageSender(reader);
        sender.start();
        
        Runtime rtime = Runtime.getRuntime();
        
        
        // comanda ex => "path -f -l sist_exp_bere.pl --goal inceput. -a 5007"
        String comanda = pathSicstus+" -f -l "+
                nume_fisier+" --goal "+
                scop+" -a "+port;
        
        //executarea procesului
        procesSicstus=rtime.exec(comanda);
        
        //InputStream-ul din care citim ce scrie procesul
        processIs=procesSicstus.getInputStream();
        
        //stream-ul de eroare
        processStreamErr=procesSicstus.getErrorStream();

    }
// </editor-fold>
    
// <editor-fold defaultstate="collapsed" desc="Getteri"> 
    
    public MainFrame getMainFrame(){
        return frame;
    }
// </editor-fold>
    
// <editor-fold defaultstate="collapsed" desc="Metode"> 
    
    void opresteProlog() throws InterruptedException{
        PipedOutputStream pos= this.sender.getPipedOutputStream();
        PrintStream ps=new PrintStream(pos);
        ps.println("exit.");
        ps.flush();
    }
// </editor-fold>
}

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package beerassistant;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Zorro Andrei
 */
public class MessageSender extends Thread{
    
// <editor-fold defaultstate="collapsed" desc="Componente"> 
    
    Socket socket ;
    MessageReader reader;
    OutputStream ostream;
    
    //volatile ca sa fie protejat la accesul concurent al mai multor threaduri
    volatile PipedOutputStream pos=null;
    volatile PipedInputStream pis;
    volatile boolean done=false;
    
// </editor-fold>

// <editor-fold defaultstate="collapsed" desc="Constructor"> 
    
    public MessageSender(MessageReader _reader) throws IOException{
        reader = _reader;
        pis = new PipedInputStream(100000);
        setPipedOutputStream(new PipedOutputStream(pis));

    }
// </editor-fold>    
    
// <editor-fold defaultstate="collapsed" desc="Setteri Sincronizati"> 
    
    public final synchronized void setPipedOutputStream(PipedOutputStream _pos){
        pos = _pos;
        notify();
    }
// </editor-fold>
    
// <editor-fold defaultstate="collapsed" desc="Getteri Sincronizati"> 
    
    public synchronized PipedOutputStream getPipedOutputStream() 
            throws InterruptedException{
        if( pos == null ){
            wait();
        }
        return pos;
    }   
// </editor-fold>
    
// <editor-fold defaultstate="collapsed" desc="Metode"> 
    
    public void trimiteMesajSicstus(String mesaj) throws InterruptedException{
        PipedOutputStream pos = getPipedOutputStream();
        PrintStream ps = new PrintStream(pos);
        ps.println(mesaj+".");
        ps.flush();
    }

    public void trimiteSirSicstus(String mesaj) throws InterruptedException{
        PipedOutputStream pos = getPipedOutputStream();
        PrintStream ps = new PrintStream(pos);
        ps.println(mesaj);
        ps.flush();
    }
    
    @Override
    public void run(){
        try {
            socket = reader.getSocket();
            ostream = socket.getOutputStream();
            int chr;
            while((chr = pis.read())!=-1){
                ostream.write(chr);
            }
            System.out.println("gata");
            
        
        } catch (IOException ex) {
            Logger.getLogger(MessageSender.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(MessageSender.class.getName()).log(Level.SEVERE, null, ex);
        }
        
    }
    
// </editor-fold>  
}

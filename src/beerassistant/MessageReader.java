/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package beerassistant;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;
/**
 *
 * @author Zorro Andrei
 */
public class MessageReader extends Thread {
    
// <editor-fold defaultstate="collapsed" desc="Componente"> 
    
    ServerSocket serverS;
    volatile Socket socket = null;//volatile ca sa fie protejat la accesul concurent al mai multor threaduri
    volatile PipedInputStream pis = null;
    ConnectionSicstus conn ;
    
// </editor-fold>
    
// <editor-fold defaultstate="collapsed" desc="Constructor"> 
    
    public MessageReader(ConnectionSicstus _conn, ServerSocket _serverS) throws IOException{
        serverS=_serverS;
        conn=_conn;
    }
// </editor-fold> 
    
// <editor-fold defaultstate="collapsed" desc="Setteri Sincronizati"> 
    
    public synchronized void setSocket(Socket _socket){
        socket =_socket;
        notify();
    }    
    
    public final synchronized void setPipedInputStream(PipedInputStream _pis){
        pis=_pis;
        notify();
    }
// </editor-fold> 
    
// <editor-fold defaultstate="collapsed" desc="Getteri Sincronizati"> 
    
    public synchronized Socket getSocket() throws InterruptedException
    {
        if (socket==null){
            wait();//asteapta pana este setat un socket
        }
        return socket;
    }
    
    public synchronized PipedInputStream getPipedInputStream() throws InterruptedException{
        if(pis==null){
            wait();
        }
        return pis;
    }
// </editor-fold>
    
// <editor-fold defaultstate="collapsed" desc="Run()"> 
    
    @Override
    public void run(){
        try {
            
            /*
                apel blocant, asteapta conexiunea
                conexiunea clinetului se face din prolog
            */
            Socket socket_aux = serverS.accept();
            setSocket(socket_aux);
            
            /* pregatesc InputStream-ul pentru a citi de pe Socket */
            InputStream is = socket_aux.getInputStream();
            
            PipedOutputStream pos=new PipedOutputStream();
            /*leg un pipedInputStream de capatul in care se scrie*/
            setPipedInputStream(new PipedInputStream(pos));
            
            int chr;
            String str = "";
            while( (chr = is.read()) != -1) {//pana nu citeste EOF
                pos.write(chr);//pun date in Pipe, primite de la Prolog
                str += (char)chr;
                if(chr == '\n'){
                    final String sirDeScris = str;
                    str = "";
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run(){ 
                            conn.getMainFrame().getDebug().append(sirDeScris); 
                            String text=sirDeScris.trim();
                            
                            /*parsarea optiunilor*/
                            
                            //verific daca e intrebare
                            if(text.length()>2 && text.charAt(0)=='i'&& text.charAt(1)=='(' && text.charAt(text.length()-1)==')')
                            {
                                String intrebare=text.substring(2, text.length()-1);
                                conn.getMainFrame().setQuestion(intrebare);
                            }
                            //verific daca sunt optiuni
                            else if(text.length()>2 && text.charAt(0)=='(' && text.charAt(text.length()-1)==')')
                            {
                                conn.getMainFrame().setOptions(text);             
                            }
                            //verific daca e solutie
                            if(text.length()>2 && text.charAt(0)=='s'&& text.charAt(1)=='(' && text.charAt(text.length()-1)==')')
                            {
                                String intrebare=text.substring(2, text.length()-1);
                                conn.getMainFrame().setSolution(intrebare);
                            }
                        }

                    });
                }
            }
            
            
        } catch (IOException ex) {
            Logger.getLogger(MessageReader.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
// </editor-fold> 
}

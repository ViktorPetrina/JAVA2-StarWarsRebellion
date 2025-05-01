package hr.vpetrina.starwars.rmi;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

public class ChatRemoteServiceImpl implements  ChatRemoteService {

    private final List<String> chatMessages;

    public ChatRemoteServiceImpl() {
        chatMessages = new ArrayList<>();
    }

    @Override
    public void sendChatMessage(String message) throws RemoteException {
        chatMessages.add(message);
    }

    @Override
    public List<String> getAllMessages() throws RemoteException {
        return chatMessages;
    }
}

package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.controller.MainBoardController;
import hr.vpetrina.starwars.jndi.ConfigurationKey;
import hr.vpetrina.starwars.jndi.ConfigurationReader;
import hr.vpetrina.starwars.model.GameState;
import javafx.application.Platform;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class NetworkUtils {

    private NetworkUtils() {}

    public static void acceptRequests(Integer port) {
        try (ServerSocket serverSocket = new ServerSocket(port)){
            while (true) {
                Socket clientSocket = serverSocket.accept();
                System.err.printf("Client connected from port %s%n", clientSocket.getPort());
                new Thread(() ->  processSerializableClient(clientSocket)).start();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void processSerializableClient(final Socket clientSocket) {
        try (ObjectInputStream ois = new ObjectInputStream(clientSocket.getInputStream());
             ObjectOutputStream oos = new ObjectOutputStream(clientSocket.getOutputStream())) {

            GameState gameState = (GameState)ois.readObject();
            GameUtils.restoreGameState(gameState);

            LogUtils.logInfo("faction turn: " + gameState.getFactionTurn());

            MainBoardController.disableControls(false);

            Platform.runLater(() -> MainBoardController.labels.getFirst().setText(
                    "Turn: " + GameState.getFactionTurnStatic())
            );

            oos.writeObject("Success");

        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void sendRequestPlayerOne(final GameState gameState) {
        try (Socket clientSocket = new Socket(
                ConfigurationReader.getStringValueForKey(ConfigurationKey.HOSTNAME),
                ConfigurationReader.getIntegerValueForKey(ConfigurationKey.PLAYER_2_SERVER_PORT))) {

            sendSerializableRequest(clientSocket, gameState);

        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void sendRequestPlayerTwo(final GameState gameState) {
        try (Socket clientSocket = new Socket(
                ConfigurationReader.getStringValueForKey(ConfigurationKey.HOSTNAME),
                ConfigurationReader.getIntegerValueForKey(ConfigurationKey.PLAYER_1_SERVER_PORT))) {

            sendSerializableRequest(clientSocket, gameState);

        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    private static void sendSerializableRequest(Socket client, GameState gameState) throws IOException, ClassNotFoundException {
        ObjectOutputStream oos = new ObjectOutputStream(client.getOutputStream());
        ObjectInputStream ois = new ObjectInputStream(client.getInputStream());
        oos.writeObject(gameState);
        LogUtils.logInfo("GameState sent to Player 2");
        LogUtils.logInfo(ois.readObject().toString());
    }

}
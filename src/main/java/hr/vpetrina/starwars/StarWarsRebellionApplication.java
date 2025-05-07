package hr.vpetrina.starwars;

import hr.vpetrina.starwars.jndi.ConfigurationKey;
import hr.vpetrina.starwars.jndi.ConfigurationReader;
import hr.vpetrina.starwars.model.GameState;
import hr.vpetrina.starwars.model.Player;
import hr.vpetrina.starwars.util.GameUtils;
import hr.vpetrina.starwars.util.NetworkUtils;
import hr.vpetrina.starwars.util.SceneUtils;
import javafx.application.Application;
import javafx.stage.Stage;

import java.io.IOException;

public class StarWarsRebellionApplication extends Application {

    @Override
    public void start(Stage stage) throws IOException {
        SceneUtils.launchScene(
                GameUtils.TITLE + " " + GameState.getCurrentPlayer(),
                SceneUtils.PICK_SIDE_WINDOW_FILE_NAME,
                800, 600);

        Thread serverThread;
        if (Player.PLAYER_ONE.equals(GameState.getCurrentPlayer())) {
            //SoundUtils.playMusic(SoundUtils.MUSIC_SOUND);
            serverThread = new Thread(() -> NetworkUtils.acceptRequests(
                    ConfigurationReader.getIntegerValueForKey(ConfigurationKey.PLAYER_1_SERVER_PORT)));
        }
        else {
            serverThread = new Thread(() -> NetworkUtils.acceptRequests(
                    ConfigurationReader.getIntegerValueForKey(ConfigurationKey.PLAYER_2_SERVER_PORT)));
        }
        serverThread.start();
    }

    public static void main(String[] args) {
        try {
            GameState.setCurrentPlayer(Player.valueOf(args[0]));
        } catch (IllegalArgumentException e) {
            return;
        }
        launch();
    }
}
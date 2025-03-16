package hr.vpetrina.starwars;

import hr.vpetrina.starwars.util.GameUtils;
import hr.vpetrina.starwars.util.SceneUtils;
import hr.vpetrina.starwars.util.SoundUtils;
import javafx.application.Application;
import javafx.stage.Stage;

import java.io.IOException;

public class StarWarsRebellionApplication extends Application {

    @Override
    public void start(Stage stage) throws IOException {
        SceneUtils.launchScene(
                GameUtils.TITLE,
                SceneUtils.PICK_SIDE_WINDOW_NAME,
                800, 600);

        SoundUtils.playMusic(SoundUtils.MUSIC_SOUND);
    }

    public static void main(String[] args) {
        launch();
    }
}
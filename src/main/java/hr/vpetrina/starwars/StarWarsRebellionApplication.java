package hr.vpetrina.starwars;

import hr.vpetrina.starwars.util.SceneUtils;
import hr.vpetrina.starwars.util.SoundUtils;
import javafx.application.Application;
import javafx.stage.Stage;

import java.io.IOException;

public class StarWarsRebellionApplication extends Application {

    @Override
    public void start(Stage stage) throws IOException, ClassNotFoundException {
        SceneUtils.launchScene(
                "Star Wars: Rebellion",
                "pick-a-side-view.fxml",
                800, 600);

        SoundUtils.playMusic("background_music.mp3");
    }

    public static void main(String[] args) {
        launch();
    }
}
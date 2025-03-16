package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.StarWarsRebellionApplication;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;

import java.util.Objects;

public class SoundUtils {

    private static MediaPlayer musicPlayer;
    private static MediaPlayer soundPlayer;

    public static void playMusic(String name) {
        String soundPath = Objects
                .requireNonNull(StarWarsRebellionApplication.class.getResource("sounds/" + name))
                .toExternalForm();
        Media media = new Media(soundPath);
        musicPlayer = new MediaPlayer(media);
        musicPlayer.play();
    }

    public static void playSound(String name) {
        String soundPath = Objects
                .requireNonNull(StarWarsRebellionApplication.class.getResource("sounds/" + name))
                .toExternalForm();
        Media media = new Media(soundPath);
        soundPlayer = new MediaPlayer(media);
        soundPlayer.play();
    }
}

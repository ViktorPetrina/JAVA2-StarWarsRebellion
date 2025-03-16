package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.StarWarsRebellionApplication;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;

import java.util.Objects;

public class SoundUtils {
    private static MediaPlayer musicPlayer;
    private static MediaPlayer soundPlayer;

    public static final String SELECT_SOUND = "select_3.mp3";
    public static final String READY_SOUND = "select_2.mp3";
    public static final String RESET_SOUND = "select_1.mp3";
    public static final String MENU_SOUND = "select_4.mp3";
    public static final String MUSIC_SOUND = "background_music.mp3";

    private SoundUtils() {}

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

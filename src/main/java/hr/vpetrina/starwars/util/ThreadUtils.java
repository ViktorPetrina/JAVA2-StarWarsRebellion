package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.GameMove;
import hr.vpetrina.starwars.thread.ReadTheLastGameMoveThread;
import hr.vpetrina.starwars.thread.SaveTheLastGameMoveThread;
import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.scene.control.Label;
import javafx.util.Duration;

public class ThreadUtils {

    private ThreadUtils() {}

    public static Timeline getTheLastEventRefreshTimeline(Label label)
    {
        Timeline showTheLastGameMoveTimeline = new Timeline(new KeyFrame(Duration.ZERO, e -> {
            ReadTheLastGameMoveThread thread = new ReadTheLastGameMoveThread(label);
            Thread theLastGameMoveThreadRunner = new Thread(thread);
            theLastGameMoveThreadRunner.start();
        }), new KeyFrame(Duration.seconds(5)));

        showTheLastGameMoveTimeline.setCycleCount(Animation.INDEFINITE);
        return showTheLastGameMoveTimeline;
    }

    public static void saveLastEvent(GameMove gameMove) {
        SaveTheLastGameMoveThread thread = new SaveTheLastGameMoveThread(gameMove);
        Thread saveTheLastGameMoveThreadRunner = new Thread(thread);
        saveTheLastGameMoveThreadRunner.start();
    }
}

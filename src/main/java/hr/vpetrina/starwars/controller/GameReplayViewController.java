package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.GameMove;
import hr.vpetrina.starwars.util.MessageUtils;
import hr.vpetrina.starwars.util.XmlUtils;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.fxml.FXML;
import javafx.scene.control.TextArea;
import javafx.util.Duration;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class GameReplayViewController {
    @FXML
    public TextArea eventsTextArea;

    @FXML
    public void initialize() {
        initializeAnimation();
    }

    private void initializeAnimation() {
        List<GameMove> gameMoves = XmlUtils.loadGameMoves();
        AtomicInteger index = new AtomicInteger(0);

        Timeline replayTimeline = new Timeline(new KeyFrame(Duration.seconds(1), _ -> {
            if (index.get() < gameMoves.size()) {
                GameMove gameMove = gameMoves.get(index.get());
                String eventDescription = MessageUtils.getIndexedEventMessage(index.get(), gameMove);
                eventsTextArea.appendText(eventDescription);
                index.getAndIncrement();
            }
        }));

        replayTimeline.setCycleCount(gameMoves.size());
        replayTimeline.play();
    }
}


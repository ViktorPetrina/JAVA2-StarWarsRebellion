package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.GameMove;
import hr.vpetrina.starwars.model.Leader;
import hr.vpetrina.starwars.model.MoveType;
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
        List<GameMove> gameMoveList = XmlUtils.loadGameMoves();

        AtomicInteger index = new AtomicInteger(0);

        Timeline replayTimeline = new Timeline(new KeyFrame(Duration.seconds(1), e -> {
            if (index.get() < gameMoveList.size()) {
                GameMove gameMove = gameMoveList.get(index.get());

                StringBuilder eventText = new StringBuilder();
                eventText.append("Move ").append(index.get() + 1).append(":\n");
                eventText.append("Planet: ").append(gameMove.getPlanet().getName()).append("\n");

                eventText.append("Leaders: ");
                if (gameMove.getLeaders().isEmpty()) {
                    eventText.append("None");
                } else {
                    for (Leader leader : gameMove.getLeaders()) {
                        eventText.append(leader.getName()).append(", ");
                    }
                    eventText.setLength(eventText.length() - 2);
                }
                eventText.append("\n");

                eventText.append("Move Type: ").append(gameMove.getMoveType().name()).append("\n");

                if (gameMove.getMoveType() == MoveType.ATTACK && gameMove.getWinner() != null) {
                    eventText.append("Winner: ").append(gameMove.getWinner().name()).append("\n");
                }

                eventText.append("Executor: ").append(gameMove.getExecutor().name()).append("\n\n");

                eventsTextArea.appendText(eventText.toString());

                index.getAndIncrement();
            }
        }));

        replayTimeline.setCycleCount(gameMoveList.size());
        replayTimeline.play();
    }
}

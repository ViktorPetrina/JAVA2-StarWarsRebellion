package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.GameMove;
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
                String eventDescription = buildEventDescription(index.get(), gameMove);
                eventsTextArea.appendText(eventDescription);
                index.getAndIncrement();
            }
        }));

        replayTimeline.setCycleCount(gameMoves.size());
        replayTimeline.play();
    }

    private String buildEventDescription(int moveIndex, GameMove gameMove) {
        StringBuilder eventText = new StringBuilder();
        eventText.append("Move ").append(moveIndex + 1).append(":\n");

        switch (gameMove.getMoveType()) {
            case ATTACK:
                eventText.append(handleAttack(gameMove));
                break;
            case SEARCH:
                eventText.append(handleSearch(gameMove));
                break;
            case DEPLOY:
                eventText.append(handleDeploy(gameMove));
                break;
            case SECRET_BASE_PICK:
                eventText.append("The REBELLION chose a planet for its secret base.\n");
                break;
            default:
                eventText.append("Unknown move.\n");
                break;
        }

        eventText.append("\n");
        return eventText.toString();
    }

    private String handleAttack(GameMove gameMove) {
        Faction executor = gameMove.getExecutor();
        Faction opponent = (executor == Faction.EMPIRE) ? Faction.REBELLION : Faction.EMPIRE;
        Faction winner = gameMove.getWinner();
        String planetName = gameMove.getPlanet().getName();
        StringBuilder text = new StringBuilder();

        text.append(executor.name())
                .append(" attacked ")
                .append(opponent.name())
                .append(" on ")
                .append(planetName)
                .append(". ");

        if (executor == winner) {
            if (!gameMove.getLeaders().isEmpty()) {
                text.append("They won and captured ")
                        .append(gameMove.getLeaders().getFirst().getName())
                        .append(".\n");
            } else {
                text.append("They won.\n");
            }
        } else {
            text.append("They lost the battle.\n");
        }
        return text.toString();
    }

    private String handleSearch(GameMove gameMove) {
        Faction winner = gameMove.getWinner();
        String planetName = gameMove.getPlanet().getName();
        StringBuilder text = new StringBuilder();

        text.append("The EMPIRE searched ")
                .append(planetName)
                .append(" for the rebel base. ");

        if (winner == Faction.EMPIRE) {
            text.append("They found the base!\n");
        } else {
            text.append("They did not find the base.\n");
        }
        return text.toString();
    }

    private String handleDeploy(GameMove gameMove) {
        Faction executor = gameMove.getExecutor();
        String planetName = gameMove.getPlanet().getName();
        StringBuilder text = new StringBuilder();

        if (!gameMove.getLeaders().isEmpty()) {
            text.append(executor.name())
                    .append(" deployed ")
                    .append(gameMove.getLeaders().getFirst().getName())
                    .append(" to ")
                    .append(planetName)
                    .append(".\n");
        } else {
            text.append(executor.name())
                    .append(" deployed to ")
                    .append(planetName)
                    .append(".\n");
        }
        return text.toString();
    }
}

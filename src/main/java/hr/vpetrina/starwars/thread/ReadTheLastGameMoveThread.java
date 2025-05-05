package hr.vpetrina.starwars.thread;

import hr.vpetrina.starwars.model.GameMove;
import hr.vpetrina.starwars.util.MessageUtils;
import javafx.application.Platform;
import javafx.scene.control.Label;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
@Data
public class ReadTheLastGameMoveThread extends AbstractGameMoveThread implements Runnable {

    private Label label;

    @Override
    public void run() {
        List<GameMove> gameMoves = loadGameMoves();
        if (!gameMoves.isEmpty()) {
            Platform.runLater(() -> label.setText(MessageUtils.getEventMessage(gameMoves.getLast())));
        }
    }
}

package hr.vpetrina.starwars.thread;

import hr.vpetrina.starwars.model.GameMove;
import hr.vpetrina.starwars.util.LogUtils;
import lombok.AllArgsConstructor;

import java.io.FileNotFoundException;

@AllArgsConstructor
public class SaveTheLastGameMoveThread extends AbstractGameMoveThread implements Runnable {

    private GameMove gameMove;

    @Override
    public void run() {
        try {
            saveTheLastGameMove(gameMove);
        } catch (FileNotFoundException e) {
            LogUtils.logSevere("An error occurred while saving the last game move!\nException: " + e);
        }
    }
}

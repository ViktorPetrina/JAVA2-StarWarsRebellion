package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.GameState;

import java.io.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class FileUtils {

    public static final String FILE_ACCESS_ERROR_MESSAGE = "Waiting during file access attempt failed!";
    private static final String SAVE_GAME_FILE_PATH = "save/game_data.dat";
    public static final String GAME_MOVE_HISTORY_FILE_NAME = "dat/gameMoves.dat";
    public static final AtomicBoolean FILE_ACCESS_IN_PROGRESS = new AtomicBoolean(false);

    private FileUtils() {}

    public static void saveGameState(GameState gameState) {
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(SAVE_GAME_FILE_PATH))) {
            oos.writeObject(gameState);
        } catch (IOException e) {
            LogUtils.logSevere(e.getMessage());
        }
    }

    public static GameState loadGameState() {
        GameState loadedGameState = null;

        try(ObjectInputStream ois = new ObjectInputStream(new FileInputStream(SAVE_GAME_FILE_PATH))) {
            loadedGameState =  (GameState) ois.readObject();
        } catch (IOException | ClassNotFoundException e) {
            LogUtils.logSevere(e.getMessage());
        }

        return loadedGameState;
    }
}

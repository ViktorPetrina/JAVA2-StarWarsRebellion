package hr.vpetrina.starwars.thread;

import hr.vpetrina.starwars.exception.ConcurrentAccessException;
import hr.vpetrina.starwars.exception.FileAccessException;
import hr.vpetrina.starwars.model.GameMove;
import hr.vpetrina.starwars.util.FileUtils;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public abstract class AbstractGameMoveThread {

    protected synchronized void saveTheLastGameMove(GameMove gameMove) throws FileNotFoundException, ConcurrentAccessException {
        while(Boolean.TRUE.equals(FileUtils.FILE_ACCESS_IN_PROGRESS.get())) {
            try {
                wait();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new ConcurrentAccessException(FileUtils.FILE_ACCESS_ERROR_MESSAGE, e);
            }
        }

        List<GameMove> gameMoves = new ArrayList<>();
        if(Files.exists(Path.of(FileUtils.GAME_MOVE_HISTORY_FILE_NAME))){
            List<GameMove> lastGameMoves = loadGameMoves();
            gameMoves.addAll(lastGameMoves);
        }

        gameMoves.add(gameMove);

        try(ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(FileUtils.GAME_MOVE_HISTORY_FILE_NAME))) {
            oos.writeObject(gameMoves);
        } catch (IOException e) {
            throw new ConcurrentAccessException("Waiting during file access attempt failed!", e);
        }

        FileUtils.FILE_ACCESS_IN_PROGRESS.set(false);
        notifyAll();
    }

    protected synchronized List<GameMove> loadGameMoves() throws ConcurrentAccessException {

        while (Boolean.TRUE.equals(FileUtils.FILE_ACCESS_IN_PROGRESS.get())) {
            try {
                wait();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new ConcurrentAccessException("Waiting during file access attempt failed!", e);
            }
        }

        FileUtils.FILE_ACCESS_IN_PROGRESS.set(true);

        List<GameMove> gameMoves = new ArrayList<>();

        if(Files.exists(Path.of(FileUtils.GAME_MOVE_HISTORY_FILE_NAME))) {
            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(FileUtils.GAME_MOVE_HISTORY_FILE_NAME))) {
                List<GameMove> lastGameMoves = (List<GameMove>) ois.readObject();
                gameMoves.addAll(lastGameMoves);
            } catch (IOException | ClassNotFoundException e) {
                throw new FileAccessException("Error while deserializing the game move history file name!", e);
            }
        }

        FileUtils.FILE_ACCESS_IN_PROGRESS.set(false);
        notifyAll();

        return gameMoves;
    }

}
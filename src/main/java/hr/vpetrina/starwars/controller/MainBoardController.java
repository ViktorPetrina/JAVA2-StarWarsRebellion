package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.util.SceneUtils;
import javafx.fxml.FXML;

import java.io.IOException;

public class MainBoardController {

    @FXML
    public void initialize() {
        try {
            SceneUtils.launch("Choose a leader", "choose-leader-view.fxml", 800, 600);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
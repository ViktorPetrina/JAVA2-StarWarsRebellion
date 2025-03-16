package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.GameState;
import hr.vpetrina.starwars.util.GameUtils;
import hr.vpetrina.starwars.util.SceneUtils;
import hr.vpetrina.starwars.util.SoundUtils;
import javafx.fxml.FXML;
import javafx.scene.control.Label;

import java.io.IOException;

public class PickASideController {
    @FXML
    public Label lblSide;

    private Faction selectedFaction;

    @FXML
    private void empireSelected() {
        SoundUtils.playSound("select_3.mp3");
        selectedFaction = Faction.EMPIRE;
        lblSide.setText("Empire selected");
    }

    @FXML
    private void rebellionSelected() {
        SoundUtils.playSound("select_3.mp3");
        selectedFaction = Faction.REBELLION;
        lblSide.setText("Rebellion selected");
    }

    @FXML
    private void ready() throws IOException {
        GameState.setPlayerOneFaction(selectedFaction);
        SoundUtils.playSound("select_2.mp3");
        if (selectedFaction == null) {
            SceneUtils.showWarningDialog(
                    "Game error",
                    "Select a Faction",
                    "A faction must be selected to start the game."
            );
            return;
        }

        SceneUtils.launchScene(
                GameUtils.TITLE,
                SceneUtils.CHOOSE_A_LEADER_WINDOW_NAME,
                1000, 800);

        SceneUtils.closeWindow(lblSide);
    }
}

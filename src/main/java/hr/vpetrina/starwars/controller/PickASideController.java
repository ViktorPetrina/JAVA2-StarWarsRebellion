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
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        selectedFaction = Faction.EMPIRE;
        lblSide.setText("Empire selected");
    }

    @FXML
    private void rebellionSelected() {
        SoundUtils.playSound(SoundUtils.SELECT_SOUND);
        selectedFaction = Faction.REBELLION;
        lblSide.setText("Rebellion selected");
    }

    @FXML
    private void ready() throws IOException {
        GameState.setPlayerFactionStatic(selectedFaction);
        SoundUtils.playSound(SoundUtils.READY_SOUND);
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
                1000, 700);

        SceneUtils.closeWindow(lblSide);
    }
}

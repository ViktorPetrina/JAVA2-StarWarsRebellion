package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.GameState;
import hr.vpetrina.starwars.util.SceneUtils;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Label;

import java.io.IOException;

public class PickASideController {
    @FXML
    public Label lblSide;

    private Faction selectedFaction;

    @FXML
    private void empireSelected() {
        selectedFaction = Faction.EMPIRE;
        lblSide.setText("Empire selected");
    }

    @FXML
    private void rebellionSelected() {
        selectedFaction = Faction.REBELLION;
        lblSide.setText("Rebellion selected");
    }

    @FXML
    private void ready() throws IOException {
        GameState.setPlayerOneFaction(selectedFaction);

        if (selectedFaction == null) {
            showUnselectedFactionDialog();
            return;
        }

        SceneUtils.launchScene(
                "Star Wars: Rebellion",
                "choose-leaders-view.fxml",
                1000, 800);

        lblSide.getScene().getWindow().hide();
    }

    private void showUnselectedFactionDialog() {
        Alert alert = new Alert(Alert.AlertType.NONE, "", ButtonType.OK);
        alert.setHeaderText("Select a Faction");
        alert.setContentText("A faction must be selected to start the game.");
        alert.showAndWait();
    }
}

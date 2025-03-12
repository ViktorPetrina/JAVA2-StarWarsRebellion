package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.util.SceneUtils;
import javafx.fxml.FXML;
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
    private void ready() {
        try {
            SceneUtils.launchScene(
                    "Star Wars: Rebellion",
                    "choose-leaders-view.fxml",
                    1000, 1200);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }


    }
}

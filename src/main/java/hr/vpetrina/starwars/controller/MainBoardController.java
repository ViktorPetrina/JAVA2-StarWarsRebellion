package hr.vpetrina.starwars.controller;

import hr.vpetrina.starwars.util.DocumentationUtils;
import hr.vpetrina.starwars.util.SceneUtils;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Pane;

import java.io.IOException;
import java.util.Objects;

public class MainBoardController {

    @FXML
    Pane menuPane;
    private Boolean menuOpened = false;

    @FXML
    public void initialize() {
        initializeMenu();
    }

    private void initializeMenu() {
        menuPane.setVisible(false);
    }

    @FXML
    private void keyPressed(KeyEvent keyEvent) {
        if (Objects.requireNonNull(keyEvent.getCode()) == KeyCode.ESCAPE) {
            openOrCloseMenu();
        }
    }

    private void openOrCloseMenu() {
        if (!menuOpened) {
            menuPane.setVisible(true);
            menuOpened = true;
        }
        else {
            menuPane.setVisible(false);
            menuOpened = false;
        }
    }

    private void showChat() {

    }

    @FXML
    private void generateDocumentation() {
        try {
            DocumentationUtils.generateDocumentation();
            SceneUtils.showInformationDialog(
                    "Documentation",
                    "Documentation generated",
                    "The documentation is successfully generated!"
            );
        } catch (IOException e) {
            SceneUtils.showWarningDialog(
                    "Documentation error",
                    "The documentation cannot be generated",
                    "Something went wrong while generating html documentation."
            );
        }
    }

    @FXML
    private void exitGame() {
        Platform.exit();
    }
}